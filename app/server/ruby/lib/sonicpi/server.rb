#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
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
require_relative "lazybuffer"
require_relative "bufferstream"
require_relative "scsynthexternal"
require_relative "thread_id"

#require_relative "scsynthnative"


module SonicPi
  class Server
    include Util

    attr_accessor :current_node_id,  :debug, :mouse_y, :mouse_x, :control_delta, :scsynth_info

    attr_reader :version

    def initialize(port, send_port, msg_queue, state, register_cue_event_lambda)
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
      @osc_path_n_order     = "/n_order".freeze
      @osc_path_n_map       = "/n_map".freeze
      @osc_path_s_new       = "/s_new".freeze
      @osc_path_b_allocread = "/b_allocRead".freeze
      @osc_path_b_alloc     = "/b_alloc".freeze
      @osc_path_b_free      = "/b_free".freeze
      @osc_path_b_write     = "/b_write".freeze
      @osc_path_b_close     = "/b_close".freeze
      @osc_path_b_info      = "/b_info".freeze
      @osc_path_b_query     = "/b_query".freeze
      @osc_path_c_set       = "/c_set".freeze
      @state = state
      @OSC_SEM = Mutex.new
      @MSG_QUEUE = msg_queue
      @control_delta = default_control_delta
      @server_thread_id = ThreadId.new(-3)
      @live_synths = {}
      @live_synths_mut = Mutex.new
      @latency = 0

      #TODO: Might want to make this available more globally so it can
      #be dynamically turned on and off
      @debug_mode = debug_mode
      @osc_events = IncomingEvents.new(:internal_events, -10)
      @scsynth = SCSynthExternal.new(@osc_events, scsynth_port: port, scsynth_send_port: send_port, register_cue_event_lambda: register_cue_event_lambda)
      @version = @scsynth.version.freeze
      @position_codes = {
        head: 0,
        tail: 1,
        before: 2,
        after: 3,
        replace: 4
      }

      @position_codes.default = :tail

      at_exit do
        puts "Exiting - shutting down scsynth server..."
        @scsynth.shutdown
      end

      request_notifications

      # Push all incoming OSC messages to the event system


      @CURRENT_NODE_ID = Counter.new(1)
      @CURRENT_SYNC_ID = Counter.new(0)
      @BUFFER_ALLOCATOR = Allocator.new(num_buffers_for_current_os)

      info_prom = Promise.new

      add_event_oneshot_handler("/sonic-pi/server-info") do |payload|
        info_prom.deliver! payload
      end
      load_synthdefs(synthdef_path)
      osc @osc_path_s_new, "sonic-pi-server-info", 1, 0, 0
      server_info = info_prom.get
      @scsynth_info = SonicPi::Core::SPMap.new({
        :sample_rate => server_info[2],
        :sample_dur => server_info[3],
        :radians_per_sample => server_info[4],
        :control_rate => server_info[5],
        :control_dur => server_info[6],
        :subsample_offset => server_info[7],
        :num_output_busses => server_info[8],
        :num_input_busses => server_info[9],
        :num_audio_busses => server_info[10],
        :num_control_busses => server_info[11],
        :num_buffers => server_info[12]
      })

      info "num input busses: #{@scsynth_info[:num_input_busses]}"
      info "num output busses: #{@scsynth_info[:num_output_busses]}"
      info "num control busses: #{@scsynth_info[:num_control_busses]}"
      info "num audio busses: #{@scsynth_info[:num_audio_busses]}"
      @AUDIO_BUS_ALLOCATOR = AudioBusAllocator.new @scsynth_info[:num_audio_busses], @scsynth_info[:num_output_busses] + @scsynth_info[:num_input_busses]
      @CONTROL_BUS_ALLOCATOR = ControlBusAllocator.new @scsynth_info[:num_control_busses]

      message "info        - Initialising comms... #{msg_queue}" if @debug_mode
      clear_scsynth!
    end

    def info(s)
      message "info        - #{s}"
    end

    def message(s)
      log "SRV #{s}"
    end

    def request_notifications
      info "Requesting notifications" if @debug_mode
      osc @osc_path_notify, 1
    end

    def load_synthdefs(path)
      info "Loading synthdefs from path: #{path}" if @debug_mode
      with_done_sync [@osc_path_d_loaddir] do
        osc @osc_path_d_loaddir, path.to_s
      end
    end

    def clear_scsynth!
      info "Clearing scsynth" if @debug_mode
      @CURRENT_NODE_ID.reset!
      @osc_events.reset!
      clear_schedule
      Kernel.sleep 0.5
      with_server_sync do
        group_clear 0, true
      end
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
        m = "unable to create a node with position: #{position} and target #{target.inspect}"
        message "nde e      - #{m}" if @debug_mode
        raise m
        nil
      end
    end

    def allocate_audio_bus
      @AUDIO_BUS_ALLOCATOR.allocate
    end

    def allocate_control_bus
      @CONTROL_BUS_ALLOCATOR.allocate
    end

    def kill_live_synth(name_id)
      synth_node = nil
      @live_synths_mut.synchronize do
        synth_node = @live_synths[name_id]
        synth_node.kill if synth_node
      end
    end

    def trigger_live_synth(name_id, position, group, synth_name, args_h, info,  now=false, t_minus_delta=false, pre_trig_blk, on_move_blk)
      @live_synths_mut.synchronize do
        pos_code = @position_codes[position]
        group_id = group.to_i
        s_name = synth_name.to_s

        normalised_args = []
        args_h.each do |k,v|
          normalised_args << k.to_s << v.to_f
        end
        initial_trigger = false
        synth_node = nil

        # Try and retrieve a cached synth node (will be here if previously triggered)
        synth_node = @live_synths[name_id]
        unless synth_node
          initial_trigger = true
          # No synth node found in cache - trigger one and cache the result
          node_id = @CURRENT_NODE_ID.next
          synth_node = SynthNode.new(node_id, group_id, self, s_name, args_h, info)
          # Call on init block if given  - this only happens the first time the synth is initiated

          # cache result
          @live_synths[name_id] = synth_node
        end


        log synth_node.stats

        orig_synth_node_group = synth_node.group

        # Call reset on synth node - this doesn't do anything if the synth
        # isn't yet in the destroyed state
        synth_node.reset!

        if initial_trigger || (group_id != orig_synth_node_group)
          pre_trig_blk.call(synth_node) if pre_trig_blk
          on_move_blk.call(synth_node) if on_move_blk


          synth_node.set_group!(group_id)

          node_id = synth_node.id
          if @debug_mode
            if osc_debug_mode
              message "lde t #{'%05d' % node_id} - Live Trigger <#{synth_name}:#{node_id}> #{position} #{group.inspect}"
            else
              message "lde t #{'%05d' % node_id} - Live Trigger <#{synth_name}:#{node_id}> #{position} #{group.inspect},  args: #{args_h}" if @debug_mode
            end
          end

          if now
            osc @osc_path_s_new, s_name, node_id, pos_code, group_id, *normalised_args

            Thread.new do
              Kernel.sleep @control_delta
              osc @osc_path_n_set, node_id, *normalised_args
              osc @osc_path_n_order, pos_code, group_id, node_id
            end

          else
            t = __system_thread_locals.get(:sonic_pi_spider_time) || Time.now
            ts =  t + sched_ahead_time
            ts = ts - @control_delta if t_minus_delta
            osc_bundle ts, @osc_path_s_new, s_name, node_id, pos_code, group_id, *normalised_args
            osc_bundle ts + @control_delta, @osc_path_n_set, node_id, *normalised_args
            osc_bundle ts + @control_delta, @osc_path_n_order, pos_code, group_id, node_id
          end
        end
        synth_node
      end
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

      normalised_args = []
      args_h.each do |k,v|
        normalised_args << k.to_s << v.to_f
      end

      if now
        osc @osc_path_s_new, s_name, node_id, pos_code, group_id, *normalised_args
      else
        t = __system_thread_locals.get(:sonic_pi_spider_time) || Time.now
        ts =  t + sched_ahead_time
        ts = ts - @control_delta if t_minus_delta
        osc_bundle ts, @osc_path_s_new, s_name, node_id, pos_code, group_id, *normalised_args
      end
      sn
    end

    def sched_ahead_time_for_node_mod(node_id)
      thread_local_time = __system_thread_locals.get(:sonic_pi_spider_time)

      if thread_local_time
        thread_local_deltas = __system_thread_locals.get(:sonic_pi_local_control_deltas)
        d = thread_local_deltas[node_id] ||= @control_delta
        thread_local_deltas[node_id] += @control_delta
        thread_local_time + d + sched_ahead_time
      else
        Time.now + sched_ahead_time
      end
    end

    def node_move(node, target_node, pos=nil, now=false)
      position = @position_codes[pos]
      message "nde m #{'%05d' % node.to_i} - Move node #{node.to_i} to: #{target_node.to_i} pos: #{position}" if @debug_mode


      if now
        osc @osc_path_n_order, position.to_i, target_node.to_i, node.to_i
      else

        ts = sched_ahead_time_for_node_mod(node.id)
        osc_bundle ts, @osc_path_n_order, position.to_i, target_node.to_i, node.to_i
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
        osc @osc_path_n_set, node_id, *normalised_args
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

    def node_map(node, args_h, now=false)
      node_id = node.to_i
      message "nde b #{'%05d' % node_id} - Map #{node.inspect}, #{args_h.inspect}" if @debug_mode

      normalised_args = []
      args_h.each do |k,v|
        normalised_args << k.to_s << v.to_f
      end

      if now
        osc @osc_path_n_map, node_id, *normalised_args
      else
        ts = sched_ahead_time_for_node_mod(node_id)
        osc_bundle ts, @osc_path_n_map, node_id, *normalised_args
      end
    end

    def control_bus_set(bus, val, now=false)
      bus = bus.to_i
      val = val.to_f

      if now
        osc @osc_path_c_set, bus, val
      else
        ts = sched_ahead_time_for_node_mod(bus * -1)
        osc_bundle ts, @osc_path_c_set, bus, val
      end
    end

    def buffer_alloc_read(path, start=0, n_frames=0)
      buffer_id = @BUFFER_ALLOCATOR.allocate
      detect_fail = lambda do |args|
        (args[0] == @osc_path_b_allocread ) && (args[2] == buffer_id)
      end
      buffer_info(buffer_id) do
        with_done_sync [@osc_path_b_allocread, buffer_id], detect_fail do
          osc @osc_path_b_allocread, buffer_id, path, start, n_frames
        end
      end
    end

    def buffer_alloc(size, n_chans=2)
      buffer_id = @BUFFER_ALLOCATOR.allocate
      buffer_info(buffer_id) do
        with_done_sync [@osc_path_b_alloc, buffer_id] do
          osc @osc_path_b_alloc, buffer_id, size, n_chans
        end
      end
    end

    def buffer_free(buf)
      osc @osc_path_b_free, buf.to_i
      @BUFFER_ALLOCATOR.release! buf.to_i
    end

    def buffer_write(buf, path, extension="wav", sample_format="int16", synchronous=true)
      path = File.expand_path(path)

      with_done_sync [@osc_path_b_write, buf.to_i] do
        osc @osc_path_b_write, buf.to_i, path, extension, sample_format, -1, 0, 0
      end
      return nil
    end

    def buffer_stream_open(path, size=65536, n_chans=2, extension="wav", sample_format="int16", n_frames=-1, start_frame=0, leave_open=1)
      buf = buffer_alloc(size, n_chans)
      buf.wait_for_allocation
      path = File.expand_path(path)
      with_done_sync [@osc_path_b_write, buf.to_i] do
        osc @osc_path_b_write, buf.to_i, path, extension, sample_format, n_frames, start_frame, leave_open
      end

      BufferStream.new(self, buf, path, size, n_chans, extension, sample_format, n_frames, start_frame, leave_open)
    end

    def buffer_stream_close(buf_stream)
      id = buf_stream.to_i
      with_done_sync [@osc_path_b_close, id] do
        osc @osc_path_b_close, id
      end
      buffer_free buf_stream
    end

    def buffer_info(id, &blk)
      prom = Promise.new
      handle_key = @osc_events.gensym("/sonicpi/server")
      @osc_events.add_handler(@osc_path_b_info, handle_key) do |payload|
        p = payload.to_a
        if (id == p[0])
          p.shift
          prom.deliver! p
          :remove_handler
        end
      end

      if blk
        Thread.new do
          res = blk.call
          if res.is_a? Exception
            prom.deliver! res
            @osc_events.rm_handler @osc_path_b_info, handle_key
          else
            osc @osc_path_b_query, id
          end
        end
      else
        osc @osc_path_b_query, id
      end
      LazyBuffer.new(self, id, prom)
    end

    def with_done_sync(matchers, detect_fail = nil, &block)
      prom = Promise.new
      handle = @osc_events.gensym("/sonicpi/server")
      fail_handle = detect_fail ? @osc_events.gensym("/sonicpi/server/fail") : nil
      if detect_fail
        @osc_events.add_handler("/fail", fail_handle) do |pl|
          pla = pl.to_a
          if detect_fail.call(pla)
            prom.deliver! Exception.new(pla[1])
            [:remove_handlers, [handle, fail_handle]]
          end
        end
      end

      @osc_events.add_handler(@osc_path_done, handle) do |pl|
        matched = true

        begin
          pla = pl.to_a
          matchers.each_with_index do |m, idx|
            if m != pla[idx]
              matched = false
              break
            end
          end
        rescue Exception => e
          matched = false
          info "with_done_sync exception:\n#{e.message}\n#{e.backtrace.inspect}\n\n"
        end
        if matched
          prom.deliver! true
          if fail_handle
            [:remove_handlers, [handle, fail_handle]]
          else
            :remove_handler
          end
        end
      end
      res = block.yield
      pres = prom.get(5)
      return pres if pres.is_a? Exception
      res
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
        rescue PromiseTimeoutError
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
      @osc_events.shutdown
    end

    def set_latency!(latency)
      @latency = latency.to_f / 1000.0
    end

    def sched_ahead_time
      sat = __system_thread_locals.get(:sonic_pi_spider_sched_ahead_time)
      return sat if sat

      t = __system_thread_locals.get(:sonic_pi_spider_time, Time.now)
      i = __system_thread_locals.get(:sonic_pi_spider_thread_id_path, @server_thread_id)
      res = @state.get(t, 0, i, 0, 0, 60, :sched_ahead_time)
      raise "sched_ahead_time, can't get time. Is this a Sonic Pi thread? " unless res
      return res.val + @latency
    end

  end
end
