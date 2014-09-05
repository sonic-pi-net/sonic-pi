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
require_relative "util"
require_relative "server"
require_relative "note"

module SonicPi
  class Studio
    include Util

    SYNTHS = ["beep", "fm", "pretty_bell", "dull_bell", "saw_beep"]
    SYNTH_MOD = Mutex.new
    SAMPLE_SEM = Mutex.new
    attr_reader :synth_group, :fx_group, :mixer_group, :recording_group, :mixer_id, :mixer_bus, :mixer, :max_concurrent_synths

    def initialize(hostname, port, msg_queue, max_concurrent_synths)
      @server = Server.new(hostname, port, msg_queue)
      @server.load_synthdefs(synthdef_path)

      @msg_queue = msg_queue
      @max_concurrent_synths = max_concurrent_synths
      @samples = {}
      @recorders = {}
      @recording_mutex = Mutex.new
      reset
    end

    def load_sample(path)
      return [@samples[path], true] if @samples[path]
      message "Loading full sample path: #{path}"
      buf_info = nil
      SAMPLE_SEM.synchronize do
        return @samples[path] if @samples[path]
        buf_info = @server.buffer_alloc_read(path)
        @samples[path] = buf_info
      end
      [buf_info, false]
    end

    def reset_and_setup_groups_and_busses
      @server.clear_scsynth!
      @mixer_bus = @server.allocate_audio_bus 2
      @mixer_group = @server.create_group(:head, 0)
      @fx_group = @server.create_group(:before, @mixer_group)
      @synth_group = @server.create_group(:before, @fx_group)
      @recording_group = @server.create_group(:after, @mixer_group)
    end

    def reset
      reset_and_setup_groups_and_busses
      start_mixer
    end




    def message(s)
      # @msg_queue.push "Studio: #{s}"
    end

    def trigger_synth(synth_name, group, args, info, now=false )
      @server.trigger_synth(:head, group, synth_name, args, info, now)
    end

    def start_mixer
      message "Starting mixer"
      @mixer = @server.trigger_synth(:head, @mixer_group, "sonic-pi-mixer", {"in_bus" => @mixer_bus})
    end

    def volume=(vol)
      message "Setting main volume to #{vol}"
      @server.node_ctl @mixer, {"amp" => vol}
    end

    def status
      @server.status
    end

    def stop
      @server.clear_schedule
      @server.group_clear @synth_group
    end

    def new_group(position, target)
      @server.create_group(position, target)
    end

    def new_synth_group
      new_group(:tail, @synth_group)
    end

    def new_fx_group
      new_group(:tail, @fx_group)
    end

    def new_fx_bus
      @server.allocate_audio_bus 2
    end

    def exit
      @server.exit
    end

    def sched_ahead_time
      @server.sched_ahead_time
    end

    def sched_ahead_time=(t)
      @server.sched_ahead_time = t
    end

    def recording_start(path, bus=0)
      return false if @recorders[bus]
      @recording_mutex.synchronize do
        return false if @recorders[bus]
        bs = @server.buffer_stream_open(path)
        s = @server.trigger_synth :head, @recording_group, "sonic-pi-recorder", {"out-buf" => bs.to_i, "in_bus" => bus}, true
        @recorders[bus] = [bs, s]
        true
      end
    end

    def recording_stop(bus=0)
      return false unless @recorders[bus]
      @recording_mutex.synchronize do
        return false unless @recorders[bus]
        bs, s = @recorders[bus]
        bs.free
        s.kill
        @recorders.delete bus
        true
      end
    end

    def load_synthdefs(path)
      @server.load_synthdefs(path)
    end
  end
end
