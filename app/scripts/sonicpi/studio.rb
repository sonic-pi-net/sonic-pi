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
    PAD_SYNTHS = ["babbling", "woah", "saws"]
    SYNTH_MOD = Mutex.new
    PAD_SEM = Mutex.new
    SAMPLE_SEM = Mutex.new
    attr_reader :synth_group, :mixer_group, :mixer_id, :mixer_bus, :pad_synth, :current_pad_synth, :mixer, :max_concurrent_synths

    def initialize(hostname, port, msg_queue, max_concurrent_synths)
      @server = Server.new(hostname, port, msg_queue)
      @server.load_synthdefs(synthdef_path)
      # Thread local variables

      Thread.current.thread_variable_set :sonic_pi_studio_current_pad_synth, nil

      @msg_queue = msg_queue
      @max_concurrent_synths = max_concurrent_synths
      @samples = {}
      reset
    end

    def load_sample(path)
      return @samples[path] if @samples[path]
      buf_info = nil
      SAMPLE_SEM.synchronize do
        return @samples[path] if @samples[path]
        buf_info = @server.buffer_alloc_read(path)
        @samples[path] = buf_info
      end
      buf_info
    end

    def reset_and_setup_groups_and_busses
      @server.clear_scsynth!
      @mixer_bus = @server.allocate_audio_bus 1
      @mixer_group = @server.create_group(:head, 0)
      @synth_group = @server.create_group(:before, @mixer_group)
    end

    def reset
      reset_and_setup_groups_and_busses
      start_mixer
    end

    def bpm
      Thread.current.thread_variable_get(:sonic_pi_studio_bpm) ||
      Thread.current.thread_variable_set(:sonic_pi_studio_bpm, 60)
    end

    def bpm=(new_bpm)
      Thread.current.thread_variable_set(:sonic_pi_studio_bpm, new_bpm)
    end

    def current_synth_name
      Thread.current.thread_variable_get(:sonic_pi_studio_current_synth_name) ||
      Thread.current.thread_variable_set(:sonic_pi_studio_current_synth_name, "pretty_bell")
    end

    def current_synth_name=(name)
      Thread.current.thread_variable_set(:sonic_pi_studio_current_synth_name, name)
    end

    def beat_s
      60.0 / bpm
    end

    def message(s)
      # @msg_queue.push "Studio: #{s}"
    end

    def trigger_synth(synth_name, group, *args)
      @server.trigger_synth(:tail, group, synth_name, "out-bus", @mixer_bus, *args)
    end

    def current_pad_synth
      Thread.current.thread_variable_get :sonic_pi_studio_current_pad_synth
    end

    def switch_to_pad(name, *args)
      if PAD_SYNTHS.include? name
        PAD_SEM.synchronize do
          current_pad_synth.kill if current_pad_synth
          message "Switching to pad #{name} with args: #{args}"
          Thread.current.thread_variable_set :sonic_pi_studio_current_pad_synth, trigger_sp_synth(name, *args)
        end
      else
        message "Unknown pad name: #{name}"
      end
    end

    def control_pad(*args)
      PAD_SEM.synchronize do
        message "Controlling pad #{@current_pad}: #{args}"
        @current_pad_synth.ctl *args if @current_pad_synth
      end
    end

    def start_mixer
      message "Starting mixer"
      @mixer = @server.trigger_synth(:head, @mixer_group, "sp/mixer", "in-bus", @mixer_bus)
    end

    def volume=(vol)
      vol = [vol, 3].min
      message "Setting volume to #{vol}"
      @server.node_ctl @mixer_group, "amp", vol
    end

    def pan=(pan)
      @server.node_ctl @mixer_group, "pan", pan
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

    def new_user_synth_group
      new_group(:tail, @synth_group)
    end

    def exit
      @server.exit
    end

    def sched_ahead_time(t)
      @server.sched_ahead_time = t
    end

  end
end
