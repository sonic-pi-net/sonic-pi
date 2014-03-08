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

module SonicPi
  class Server
    include Util

    attr_accessor :current_node_id,  :debug, :mouse_y, :mouse_x, :sched_ahead_time

    def initialize(hostname, port, msg_queue)
      @sched_ahead_time = 0.1
      @OSC_SEM = Mutex.new
      @HOSTNAME = hostname

      @MSG_QUEUE = msg_queue
      @control_delta = 0.005


      @PORT = port
      @CLIENT = OSC::Server.new(4800)
      @EVENTS = IncomingEvents.new


      # Push all incoming OSC messages to the event system
      @CLIENT.add_method '*' do |m|
        @EVENTS.event m.address, m.to_a
      end

      @CURRENT_NODE_ID = Counter.new(1)
      @CURRENT_BUFFER_ID = Counter.new(0)
      @CURRENT_SYNC_ID = Counter.new(0)

      @AUDIO_BUS_ALLOCATOR = AudioBusAllocator.new 100, 10 #TODO: remove these magic nums
      @CONTROL_BUS_ALLOCATOR = ControlBusAllocator.new 1000, 0

      @SERVER_THREAD = Thread.new do
        log "starting server thread"
        @CLIENT.run
      end

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
      osc "/d_loadDir", path.to_s
      sleep 2 ## TODO: replace me with a blocking wait (for a callback)
    end

    def clear_scsynth!
      message "Clearing scsynth"
      @CURRENT_NODE_ID.reset!
      clear_schedule
      group_clear 0
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

    def group_clear(id)
      message "Clearing (nuking) group #{id}"
      osc "/g_freeAll", id.to_f
    end

    def group_deep_free(id)
      message "Deep freeing group #{id}"
      osc "/g_deepFree", id.to_f
    end

    def kill_node(id)
      message "Killing node #{id}"
      osc "/n_free", id.to_f
    end

    def create_group(position, target)
      target_id = target.to_i
      pos_code = position_code(position)
      id = @CURRENT_NODE_ID.next
      if (pos_code && target_id)
        message "Group created with id: #{id}"
        osc "/g_new", id.to_f, pos_code.to_f, target_id.to_f
        Group.new id, self
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

    def trigger_synth(position, group, synth_name, *args)
      message "Triggering synth #{synth_name} at #{position}, #{group.to_s}"
      pos_code = position_code(position)
      group_id = group.to_i
      node_id = @CURRENT_NODE_ID.next
      sn = SynthNode.new(node_id.to_f, self, synth_name.to_s)
      normalised_args = []
      args.each_slice(2){|el| normalised_args.concat([el.first.to_s, el[1].to_f])}
      ts = (Thread.current.thread_variable_get(:sonic_pi_spider_time) || Time.now) + @sched_ahead_time

      osc_bundle ts, "/s_new", synth_name.to_s, node_id.to_f, pos_code.to_f, group_id.to_f, *normalised_args
      sn
    end

    def sched_osc_for_node(node, path, *args)
      thread_local_time = Thread.current.thread_variable_get(:sonic_pi_spider_time)

      if thread_local_time
        thread_local_deltas = Thread.current.thread_variable_get(:sonic_pi_control_deltas)
        thread_local_deltas[node] ||= 0
        d = thread_local_deltas[node] += @control_delta
        ts = thread_local_time + d + @sched_ahead_time
      else
        ts = Time.now + @sched_ahead_time
      end

      osc_bundle ts, path, *args

    end

    def node_ctl(node, *args)
      message "controlling node: #{node} with args: #{args}"

      normalised_args = []
      args.each_slice(2){|el| normalised_args.concat([el.first.to_s, el[1].to_f])}

      sched_osc_for_node(node, "/n_set", node.to_f, *normalised_args)

    end

    def buffer_alloc_read(path, start=0, n_frames=0)
      buffer_id = @CURRENT_BUFFER_ID.next
      with_done_sync do
        osc "/b_allocRead", buffer_id, path, start, n_frames
      end
      buffer_info(buffer_id)
    end

    def buffer_info(id)
      prom = Promise.new
      @EVENTS.add_handler("/b_info", @EVENTS.gensym("/sonicpi/server")) do |payload|
        if (id == payload.to_a[0])
          prom.deliver!  payload
          :remove_handler
        end
      end
      osc "/b_query", id
      res = prom.get

      args = res.to_a
      Buffer.new(args[0], args[1], args[2], args[3])
    end

    def with_done_sync(&block)
      with_server_sync do
        prom = Promise.new
        @EVENTS.add_handler("/done", @EVENTS.gensym("/sonicpi/server")) do |pl|
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
      @EVENTS.add_handler("/synced", @EVENTS.gensym("/sonicpi/server")) do |payload|
        if (id == payload.to_a[0])
          prom.deliver!  true
          :remove_handler
        end
      end
      osc "/sync", id.to_f
      res = block.yield
      prom.get
      res
    end

    def status
      prom = Promise.new
      res = nil

      @EVENTS.oneshot_handler("/status.reply") do |pl|
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
        :act_samp_rate => args[8]
      }
    end

    def osc(*args)
      message "--> osc: #{args}"
      @CLIENT.send(OSC::Message.new(*args), @HOSTNAME, @PORT)
    end

    def osc_bundle(ts, *args)
      m = OSC::Message.new(*args)
      b = OSC::Bundle.new(ts, m)
      @CLIENT.send(b, @HOSTNAME, @PORT)
    end

    def add_event_handler(handle, key, &block)
      @EVENTS.add_handler(handle, key, &block)
    end

    def add_event_oneshot_handler(handle, &block)
      @EVENTS.oneshot_handler(handle, &block)
    end

    def rm_event_handler(handle, key)
      @EVENTS.rm_handler(handle, key)
    end

    def event_gensym(s)
      @EVENTS.gensym(s)
    end

    def exit
      osc "/quit"
      @SERVER_THREAD.kill
    end

  end
end
