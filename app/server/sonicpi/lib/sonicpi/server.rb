#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
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

    attr_accessor :current_node_id,  :debug, :mouse_y, :mouse_x, :sched_ahead_time, :control_delta

    def initialize(hostname, port, msg_queue)
      # Cache common OSC path strings as frozen instance
      # vars to reduce object creation cost and GC load
      @osc_path_quit        = "/quit".freeze
      @osc_path_status      = "/status".freeze
      @osc_path_sync        = "/sync".freeze
      @osc_path_synced      = "/synced".freeze
      @osc_path_done        = "/done".freeze
      @osc_path_notify      = "/notify".freeze
      @osc_path_clearsched  = "/clearSched"
      @osc_path_d_loaddir   = "/d_loadDir".freeze
      @osc_path_g_freeall   = "/g_freeAll".freeze
      @osc_path_g_deepfree  = "/g_deepFree".freeze
      @osc_path_g_new       = "/g_new".freeze
      @osc_path_n_free      = "/n_free".freeze
      @osc_path_n_set       = "/n_set".freeze
      @osc_path_n_run       = "/n_run".freeze
      @osc_path_s_new       = "/s_new".freeze
      @osc_path_b_allocread = "/b_allocRead".freeze
      @osc_path_b_alloc     = "/b_alloc".freeze
      @osc_path_b_free      = "/b_free".freeze
      @osc_path_b_write     = "/b_write".freeze
      @osc_path_b_close     = "/b_close".freeze
      @osc_path_b_info      = "/b_info".freeze
      @osc_path_b_query     = "/b_query".freeze

      @OSC_SEM = Mutex.new
      @HOSTNAME = hostname
      @sched_ahead_time = default_sched_ahead_time
      @MSG_QUEUE = msg_queue
      @control_delta = default_control_delta

      #TODO: Might want to make this available more globally so it can
      #be dynamically turned on and off
      @debug_mode = debug_mode

      @PORT = port

      @osc_events = IncomingEvents.new(:internal_events, -10)

      @scsynth = SCSynthExternal.new(@osc_events)

      @position_codes = {
        head: 0,
        tail: 1,
        before: 2,
        after: 3,
        replace: 4
      }

      at_exit do
        log "Exiting - shutting down scsynth server..."
        @scsynth.shutdown
      end

      # Push all incoming OSC messages to the event system


      @CURRENT_NODE_ID = Counter.new(1)
      @CURRENT_SYNC_ID = Counter.new(0)
      @BUFFER_ALLOCATOR = Allocator.new(1024) # TODO: Another magic num to remove
      @AUDIO_BUS_ALLOCATOR = AudioBusAllocator.new num_audio_busses_for_current_os, 10 #TODO: remove these magic nums
      @CONTROL_BUS_ALLOCATOR = ControlBusAllocator.new 4096

      message "info        - Initialising comms... #{msg_queue}" if @debug_mode
      clear_scsynth!
      request_notifications


    end

    def message(s)
      log "SRV #{s}"
    end

   def request_notifications
      message "info        - Requesting notifications" if @debug_mode
      osc @osc_path_notify, 1
    end

    def load_synthdefs(path)
      message "info        - Loading synthdefs from path: #{path}" if @debug_mode
      with_server_sync do
        osc @osc_path_d_loaddir, path.to_s
      end
    end

    def clear_scsynth!
      message "info        - Clearing scsynth" if @debug_mode
      @CURRENT_NODE_ID.reset!
      clear_schedule
      group_clear 0, true
    end

    def clear_schedule
      osc @osc_path_clearsched
    end

    def reset!
      clear_schedule
      clear_scsynth!
      @AUDIO_BUS_ALLOCATOR.reset!
      @CONTROL_BUS_ALLOCATOR.reset!
    end

    def group_clear(id, now=false)
      message "grp f #{'%05d' % id} - Clear #{id.inspect}" if @debug_mode
      id = id.to_i
      if now
        osc @osc_path_g_freeall, id
      else
        ts = sched_ahead_time_for_node_mod(id)
        osc_bundle ts, @osc_path_g_freeall, id
      end
    end

    def group_deep_free(id, now=false)
      message "grp d #{'%05d' % id} - Deep free #{id}" if @debug_mode
      id = id.to_i
      if now
        osc @osc_path_g_deepfree, id
      else
        ts = sched_ahead_time_for_node_mod(id)
        osc_bundle ts, @osc_path_g_deepfree, id
      end
    end

    def kill_node(id, now=false)
       if @debug_mode
         if id.is_a? Group
           message "grp k #{'%05d' % id} - Kill #{id.inspect}"
         else
           message "nde k #{'%05d' % id} - Kill #{id.inspect}"
         end
       end

      id = id.to_i
      if now
        osc @osc_path_n_free, id
      else
        ts = sched_ahead_time_for_node_mod(id)
        osc_bundle ts, @osc_path_n_free, id
      end
    end

    def create_group(position, target, name="")
      target_id = target.to_i
      pos_code = @position_codes[position]
      id = @CURRENT_NODE_ID.next
      if (pos_code && target_id)
        g = Group.new id, self, name
        osc @osc_path_g_new, id, pos_code, target_id
        message "grp n #{'%05d' % id} - Create [#{name}:#{id}] #{position} #{target.inspect}" if @debug_mode
        g.wait_until_started
      else
        message "nde e      - unable to create a node with position: #{position} and target #{target.inspect}" if @debug_mode
        nil
      end
    end

    def allocate_audio_bus
      @AUDIO_BUS_ALLOCATOR.allocate
    end

    def allocate_control_bus
      @CONTROL_BUS_ALLOCATOR.allocate
    end



    def trigger_synth(position, group, synth_name, args_h, info=nil, now=false, t_minus_delta=false)
      pos_code = @position_codes[position]
      group_id = group.to_i
      node_id = @CURRENT_NODE_ID.next
      if @debug_mode
        if osc_debug_mode
          message "nde t #{'%05d' % node_id} - Trigger <#{synth_name}:#{node_id}> #{position} #{group.inspect}"
        else
          message "nde t #{'%05d' % node_id} - Trigger <#{synth_name}:#{node_id}> #{position} #{group.inspect},  args: #{args_h}" if @debug_mode
        end
      end

      s_name = synth_name.to_s
      sn = SynthNode.new(node_id, group, self, s_name, args_h, info)

      group.subnode_add(sn)

      sn.on_destroyed do
        group.subnode_rm(sn)
      end

      if now
        osc @osc_path_s_new, s_name, node_id, pos_code, group_id, *args_h.flatten
      else
        t = Thread.current.thread_variable_get(:sonic_pi_spider_time) || Time.now
        ts =  t + @sched_ahead_time
        ts = ts - @control_delta if t_minus_delta
        osc_bundle ts, @osc_path_s_new, s_name, node_id, pos_code, group_id, *args_h.flatten
      end
      sn
    end

    def sched_ahead_time_for_node_mod(node_id)
      thread_local_time = Thread.current.thread_variable_get(:sonic_pi_spider_time)

      if thread_local_time
        thread_local_deltas = Thread.current.thread_variable_get(:sonic_pi_control_deltas)
        d = thread_local_deltas[node_id] ||= @control_delta
        thread_local_deltas[node_id] += @control_delta
        thread_local_time + d + @sched_ahead_time
      else
        Time.now + @sched_ahead_time
      end
    end

    def node_ctl(node, args, now=false)
      args_h = resolve_synth_opts_hash_or_array(args)
      node_id = node.to_i
      message "nde c #{'%05d' % node_id} - Control #{node.inspect} with args: #{args}" if @debug_mode

      normalised_args = []
      args_h.each do |k,v|
        normalised_args << k.to_s << v.to_f
      end

      if now
        osc @osc_path_n_set, node.to_i, *normalised_args
      else
        ts = sched_ahead_time_for_node_mod(node_id)
        osc_bundle ts, @osc_path_n_set, node_id, *normalised_args
      end
    end

    def node_pause(node, now=false)
      node_id = node.to_i
      message "nde p #{'%05d' % node_id} - Pause #{node.inspect}" if @debug_mode

      if now
        osc @osc_path_n_run, node_id, 0
      else
        ts = sched_ahead_time_for_node_mod(node_id)
        osc_bundle ts, @osc_path_n_run, node_id, 0
      end
    end

    def node_run(node, now=false)
      node_id = node.to_i
      message "nde r #{'%05d' % node_id} - Run #{node.inspect}" if @debug_mode

      if now
        osc @osc_path_n_run, node_id, 1
      else
        ts = sched_ahead_time_for_node_mod(node_id)
        osc_bundle ts, @osc_path_n_run, node_id, 1
      end
    end

    def buffer_alloc_read(path, start=0, n_frames=0)
      buffer_id = @BUFFER_ALLOCATOR.allocate
      # TODO do we need to sync these?
      with_done_sync do
        osc @osc_path_b_allocread, buffer_id, path, start, n_frames
      end
      buffer_info(buffer_id)
    end

    def buffer_alloc(size, n_chans=2)
      buffer_id = @BUFFER_ALLOCATOR.allocate
      with_done_sync do
        osc @osc_path_b_alloc, buffer_id, size, n_chans
      end
      buffer_info(buffer_id)
    end

    def buffer_free(buf)
      with_done_sync do
        osc @osc_path_b_free, buf.to_i
      end

      @BUFFER_ALLOCATOR.release! buf.to_i
    end

    def buffer_stream_open(path, size=65536, n_chans=2, extension="wav", sample_format="int16", n_frames=-1, start_frame=0, leave_open=1)
      buf = buffer_alloc(size, n_chans)
      path = File.expand_path(path)
      with_done_sync do
        osc @osc_path_b_write, buf.to_i, path, extension, sample_format, n_frames, start_frame, leave_open
      end

      BufferStream.new(self, buf, path, size, n_chans, extension, sample_format, n_frames, start_frame, leave_open)
    end

    def buffer_stream_close(buf_stream)
      with_done_sync do
        osc @osc_path_b_close, buf_stream.to_i
      end
      buffer_free buf_stream
    end

    def buffer_info(id)
      prom = Promise.new
      @osc_events.add_handler(@osc_path_b_info, @osc_events.gensym("/sonicpi/server")) do |payload|
        if (id == payload.to_a[0])
          prom.deliver!  payload
          :remove_handler
        end
      end
      osc @osc_path_b_query, id
      res = prom.get

      args = res.to_a
      Buffer.new(self, args[0], args[1], args[2], args[3])
    end

    def with_done_sync(&block)
      with_server_sync do
        prom = Promise.new
        @osc_events.add_handler(@osc_path_done, @osc_events.gensym("/sonicpi/server")) do |pl|
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
      @osc_events.add_handler(@osc_path_synced, @osc_events.gensym("/sonicpi/server")) do |payload|
        if (id == payload.to_a[0])
          prom.deliver!  :sonic_pi_server_sync_notification
          :remove_handler
        end
      end
      res = block.yield
      osc @osc_path_sync, id
      prom.get
      res
    end

    def status(timeout=nil)
      prom = Promise.new
      res = nil

      key = @osc_events.gensym("/status.reply")
      @osc_events.async_add_handler("/status.reply", key) do |pl|
        prom.deliver! pl
        :remove_handler
      end

      with_server_sync do
        osc @osc_path_status
        begin
          res = prom.get(timeout)
        rescue PromiseTimeoutError => e
          @osc_events.rm_handler("/status.reply", key)
          return nil
        end
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
#      log "--> osc: #{args}"
      @scsynth.send(*args)
    end

    def osc_bundle(ts, *args)
#      log "--> oscb at #{ts}, #{args}"
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

    def shutdown
      @scsynth.shutdown
    end

  end
end
