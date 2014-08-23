#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++
require 'osc-ruby'
require_relative "util"
require_relative "group"
require_relative "synthnode"
require_relative "audiobusallocator"
require_relative "controlbusallocator"
require_relative "promise"
require_relative "incomingevents"
require_relative "counter"
require_relative "buffer"
require_relative "bufferstream"
require_relative "scsynthexternal"
#require_relative "scsynthnative"


module SonicPi
  class Server
    include Util

    attr_accessor :current_node_id,  :debug, :mouse_y, :mouse_x, :sched_ahead_time

    def initialize(hostname, port, msg_queue)
      @OSC_SEM = Mutex.new
      @HOSTNAME = hostname
      @sched_ahead_time = default_sched_ahead_time
      @MSG_QUEUE = msg_queue
      @control_delta = 0.005

      @PORT = port

      @osc_events = IncomingEvents.new

      @scsynth = SCSynthExternal.new do |m, args|

        case m
        when "/n_end"
          id = args[0].to_i
          @osc_events.async_event "/n_end/#{id}", args
        when "/n_off"
          id = args[0].to_i
          @osc_events.async_event "/n_off/#{id}", args
        when "/n_on"
          id = args[0].to_i
          @osc_events.async_event "/n_on/#{id}", args
        when "/n_go"
          id = args[0].to_i
          @osc_events.async_event "/n_go/#{id}", args
        else
          @osc_events.async_event m, args
        end
      end

      at_exit do
        puts "Exiting - shutting down scsynth server..."
        @scsynth.shutdown
      end

      # Push all incoming OSC messages to the event system


      @CURRENT_NODE_ID = Counter.new(1)
      @CURRENT_SYNC_ID = Counter.new(0)
      @BUFFER_ALLOCATOR = Allocator.new(1024) # TODO: Another magic num to remove
      @AUDIO_BUS_ALLOCATOR = AudioBusAllocator.new 128, 10 #TODO: remove these magic nums
      @CONTROL_BUS_ALLOCATOR = ControlBusAllocator.new 4096

      message "Initialising comms... #{msg_queue}"
      clear_scsynth!
      request_notifications

    end

    def message(s)
      @MSG_QUEUE.push({:type => :debug_message, :val => s}) if debug_mode
    end

    def request_notifications
      message "Requesting notifications"
      osc "/notify", 1
    end

    def load_synthdefs(path)
      message "Loading synthdefs from path: #{path}"
      with_server_sync do
        osc "/d_loadDir", path.to_s
      end
    end

    def clear_scsynth!
      message "Clearing scsynth"
      @CURRENT_NODE_ID.reset!
      clear_schedule
      group_clear 0, true
    end

    def clear_schedule
      osc "/clearSched"
    end

    def reset!
      clear_schedule
      clear_scsynth!
      @AUDIO_BUS_ALLOCATOR.reset!
      @CONTROL_BUS_ALLOCATOR.reset!
    end

    def position_code(position)
      {
        head: 0,
        tail: 1,
        before: 2,
        after: 3,
        replace: 4}[position]
    end

    def group_clear(id, now=false)
      message "Clearing (nuking) group #{id}"
      if now
        osc "/g_freeAll", id.to_f
      else
        ts = sched_ahead_time_for_node(id)
        osc_bundle ts, "/g_freeAll", id.to_f
      end
    end

    def group_deep_free(id, now=false)
      message "Deep freeing group #{id}"
      if now
        osc "/g_deepFree", id.to_f
      else
        ts = sched_ahead_time_for_node(id)
        osc_bundle ts, "/g_deepFree", id.to_f
      end
    end

    def kill_node(id, now=false)
      message "Killing node #{id}"
      if now
        osc "/n_free", id.to_f
      else
        ts = sched_ahead_time_for_node(id)
        osc_bundle ts, "/n_free", id.to_f
      end
    end

    def create_group(position, target)
      target_id = target.to_i
      pos_code = position_code(position)
      id = @CURRENT_NODE_ID.next
      if (pos_code && target_id)
        g = Group.new id, self
        osc "/g_new", id.to_f, pos_code.to_f, target_id.to_f
        message "Group created with id: #{id}"
        g.wait_until_started
      else
        message "Unable to create a node with position: #{position} and target #{target}"
        nil
      end
    end

    def allocate_audio_bus(num_chans)
      @AUDIO_BUS_ALLOCATOR.allocate num_chans
    end

    def allocate_control_bus(num_chans)
      @CONTROL_BUS_ALLOCATOR.allocate num_chans
    end



    def trigger_synth(position, group, synth_name, args_h, info=nil, now=false)
      message "Triggering synth #{synth_name} at #{position}, #{group.to_s}" if debug_mode
      pos_code = position_code(position)
      group_id = group.to_i
      node_id = @CURRENT_NODE_ID.next

      normalised_args = []
      normalised_args_map = {}
      args_h.each do |k,v|
        ks = k.to_s
        vf = v.to_f
        normalised_args << ks << vf
        normalised_args_map[ks] = vf
      end

      s_name = synth_name.to_s
      sn = SynthNode.new(node_id, group, self, s_name, normalised_args_map, info)

      group.subnode_add(sn)

      sn.on_destroyed do
        group.subnode_rm(sn)
      end

      if now
        osc "/s_new", s_name, node_id, pos_code, group_id, *normalised_args
      else
        ts = sched_ahead_time_for_node(sn)
        osc_bundle ts, "/s_new", s_name, node_id, pos_code, group_id, *normalised_args
      end
      sn
    end

    def sched_ahead_time_for_node(node)
      node_id = node.to_i
      thread_local_time = Thread.current.thread_variable_get(:sonic_pi_spider_time)

      if thread_local_time
        thread_local_deltas = Thread.current.thread_variable_get(:sonic_pi_control_deltas)
        d = thread_local_deltas[node_id] ||= 0
        thread_local_deltas[node_id] += @control_delta
        thread_local_time + d + @sched_ahead_time
      else
        Time.now + @sched_ahead_time
      end
    end

    def node_ctl(node, args, now=false)
      args_h = resolve_synth_opts_hash_or_array(args)
      message "controlling node: #{node} with args: #{args}" if debug_mode

      normalised_args = []
      args_h.each do |k,v|
        normalised_args << k.to_s << v.to_f
      end

      if now
        osc "/n_set", node.to_i, *normalised_args
      else
        ts = sched_ahead_time_for_node(node)
        osc_bundle ts, "/n_set", node.to_i, *normalised_args
      end
    end

    def node_pause(node, now=false)
      message "pausing node: #{node}"
      if now
        osc "/n_run", node.to_i, 0
      else
        ts = sched_ahead_time_for_node(node)
        osc_bundle ts, "/n_run", node.to_i, 0
      end
    end

    def node_run(node, now=false)
      message "running node: #{node}"
      if now
        osc "/n_run", node.to_i, 1
      else
        ts = sched_ahead_time_for_node(node)
        osc_bundle ts, "/n_run", node.to_i, 1
      end
    end

    def buffer_alloc_read(path, start=0, n_frames=0)
      buffer_id = @BUFFER_ALLOCATOR.allocate
      with_done_sync do
        osc "/b_allocRead", buffer_id, path, start, n_frames
      end
      buffer_info(buffer_id)
    end

    def buffer_alloc(size, n_chans=2)
      buffer_id = @BUFFER_ALLOCATOR.allocate
      with_done_sync do
        osc "/b_alloc", buffer_id, size, n_chans
      end
      buffer_info(buffer_id)
    end

    def buffer_free(buf)
      with_done_sync do
        osc "/b_free", buf.to_i
      end

      @BUFFER_ALLOCATOR.release! buf.to_i
    end

    def buffer_stream_open(path, size=65536, n_chans=2, extension="wav", sample_format="int16", n_frames=-1, start_frame=0, leave_open=1)
      buf = buffer_alloc(size, n_chans)
      path = File.expand_path(path)
      with_done_sync do
        osc "/b_write", buf.to_i, path, extension, sample_format, n_frames, start_frame, leave_open
      end

      BufferStream.new(self, buf, path, size, n_chans, extension, sample_format, n_frames, start_frame, leave_open)
    end

    def buffer_stream_close(buf_stream)
      with_done_sync do
        osc "/b_close", buf_stream.to_i
      end
      buffer_free buf_stream
    end

    def buffer_info(id)
      prom = Promise.new
      @osc_events.add_handler("/b_info", @osc_events.gensym("/sonicpi/server")) do |payload|
        if (id == payload.to_a[0])
          prom.deliver!  payload
          :remove_handler
        end
      end
      osc "/b_query", id
      res = prom.get

      args = res.to_a
      Buffer.new(self, args[0], args[1], args[2], args[3])
    end

    def with_done_sync(&block)
      with_server_sync do
        prom = Promise.new
        @osc_events.add_handler("/done", @osc_events.gensym("/sonicpi/server")) do |pl|
          prom.deliver! true
          :remove_handler
        end
        res = block.yield
        prom.get
        res
      end
    end

    def with_server_sync(&block)
      id = @CURRENT_SYNC_ID.next
      prom = Promise.new
      @osc_events.add_handler("/synced", @osc_events.gensym("/sonicpi/server")) do |payload|
        if (id == payload.to_a[0])
          prom.deliver!  :sonic_pi_server_sync_notification
          :remove_handler
        end
      end
      res = block.yield
      osc "/sync", id.to_f
      prom.get
      res
    end

    def status
      prom = Promise.new
      res = nil

      @osc_events.oneshot_handler("/status.reply") do |pl|
        prom.deliver! pl
      end

      with_server_sync do
        osc "/status"
        res = prom.get
      end

      args = res.to_a
      {
        :ugens => args[1],
        :synths => args[2],
        :groups => args[3],
        :sdefs => args[4],
        :avg_cpu => args[5],
        :peak_cpu => args[6],
        :nom_samp_rate => args[7],
        :act_samp_rate => args[8],
        :audio_busses => @AUDIO_BUS_ALLOCATOR.num_busses_allocated,
        :control_busses => @CONTROL_BUS_ALLOCATOR.num_busses_allocated
      }
    end

    def osc(*args)
#      Kernel.puts "--> osc: #{args}"
      @scsynth.send(*args)
    end

    def osc_bundle(ts, *args)
#      Kernel.puts "--> osc at #{ts}, #{args}"
      @scsynth.send_at(ts, *args)
    end

    def async_add_event_handlers(*args)
      @osc_events.async_add_handlers(*args)
    end

    def add_event_handler(handle, key, &block)
      @osc_events.add_handler(handle, key, &block)
    end

    def async_add_event_handler(handle, key, &block)
      @osc_events.async_add_handler(handle, key, &block)
    end

    def add_event_oneshot_handler(handle, &block)
      @osc_events.oneshot_handler(handle, &block)
    end

    def rm_event_handler(handle, key)
      @osc_events.rm_handler(handle, key)
    end

    def event_gensym(s)
      @osc_events.gensym(s)
    end

    def event(handle, payload)
      @osc_events.event handle, payload
    end

    def async_event(handle, payload)
      @osc_events.async_event handle, payload
    end

    def exit
      osc "/quit"
    end

  end
end
