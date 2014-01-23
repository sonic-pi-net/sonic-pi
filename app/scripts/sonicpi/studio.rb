require File.absolute_path("#{File.dirname(__FILE__)}/util")
require File.absolute_path("#{File.dirname(__FILE__)}/server")

module SonicPi
  class Studio
    include Util

    SYNTHS = ["beep", "fm", "pretty_bell", "dull_bell", "saw_beep"]
    PAD_SYNTHS = ["babbling", "woah", "saws"]
    SYNTH_MOD = Mutex.new
    PAD_SEM = Mutex.new
    SAMPLE_SEM = Mutex.new
    attr_accessor :bpm, :current_synth_name
    attr_reader :synth_group, :mixer_group, :mixer_id, :mixer_bus, :pad_synth, :current_pad_synth, :mixer, :max_concurrent_synths

    def initialize(hostname, port, msg_queue, max_concurrent_synths)
      @server = Server.new(hostname, port, msg_queue)
      @bpm = 60
      @current_synth_name = "pretty_bell"
      @msg_queue = msg_queue
      @running_synths = []
      @max_concurrent_synths = max_concurrent_synths
      @samples = {}
      reset
    end

    def load_sample(path)
      return @samples[path] if @samples[path]
      buf_info = nil
      SAMPLE_SEM.synchronize do
        id = @server.buffer_alloc_read(path)
        buf_info = @server.buffer_info(id)
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

    def beat_s
      60.0 / @bpm
    end

    def message(s)
      # @msg_queue.push "Studio: #{s}"
    end

    def trigger_synth(synth_name, *args)
      trigger_non_sp_synth("sp/#{synth_name}", *args)
    end

    def trigger_non_sp_synth(synth_name, *args)
      @server.trigger_synth(:tail, @synth_group, synth_name, "out-bus", @mixer_bus, *args)
    end



    def switch_to_pad(name, *args)
      full_name = "sp/#{name}"
      if PAD_SYNTHS.include? name
        PAD_SEM.synchronize do
          @current_pad_synth.kill if @current_pad_synth
          message "Switching to pad #{name} with args: #{args}"
          @current_pad_synth = trigger_synth :head, @synth_group, full_name, "out-bus", @mixer_bus, *args
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
      @mixer = @server.trigger_synth(:head, @mixer_group, "sp/mixer", "in-bus", @mixer_bus, "pan", 0)
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

    private

    def kill_old_synths
      SYNTH_MOD.synchronize do

      end
    end

  end
end
