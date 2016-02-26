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
require_relative "server"
require_relative "note"

require 'set'

module SonicPi
  class Studio

    class StudioCurrentlyRebootingError < StandardError ; end
    include Util

    attr_reader :synth_group, :fx_group, :mixer_group, :recording_group, :mixer_id, :mixer_bus, :mixer, :max_concurrent_synths, :rand_buf_id, :amp, :rebooting

    attr_accessor :cent_tuning

    def initialize(hostname, port, msg_queue, max_concurrent_synths)
      @hostname = hostname
      @port = port
      @msg_queue = msg_queue
      @max_concurrent_synths = max_concurrent_synths
      @error_occured_mutex = Mutex.new
      @error_occurred_since_last_check = false
      @sample_sem = Mutex.new
      @reboot_mutex = Mutex.new
      @rebooting = false
      @cent_tuning = 0
      @reboot_queue = Queue.new
      @reboot_queue << :check

      init_studio
      reset_server

      @server_rebooter = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, "server checker")
        Thread.current.priority = 300
        Kernel.sleep 10
        loop do
          vs = []
          vs << @reboot_queue.pop
          # drain any other messages
          @reboot_queue.size.times {vs << @reboot_queue.pop}
          begin
            if vs.include? :reboot
              begin
                server_reboot
                Kernel.sleep 10
                @reboot_queue << :check
              rescue Exception => e
                message "Error rebooting server:  #{e}, #{e.backtrace}"
                message "Attempting to reboot again in 10s"
                begin
                  message "Forcing shutdown of any running server"
                  @server.shutdown
                rescue Exception => e
                  message "Error shutting down server:  #{e}, #{e.backtrace}"
                end
                Kernel.sleep 10
                @reboot_queue << :reboot
              end
            else
              begin
                if @server.status(5)
                  # server is alive
                  # check again in 5 seconds...
                  Thread.new do
                    Kernel.sleep 5
                    @reboot_queue << :check
                  end
                else
                  message "Sound server is down. Rebooting..."
                  @reboot_queue << :reboot
                end
              rescue Exception => e
                message "Error communicating with sound server. Rebooting...  #{e}, #{e.backtrace}"
                @reboot_queue <<  :reboot
              end
            end
          rescue Exception => e
            message "Error in reboot thread: #{e}, #{e.backtrace}"
          end
        end
      end
    end

    def init_studio
      message "Initializing..."
      @amp = [0.0, 1.0]

      server = Server.new(@hostname, @port, @msg_queue)
      server.load_synthdefs(synthdef_path)
      server.add_event_handler("/sonic-pi/amp", "/sonic-pi/amp") do |payload|
        @amp = [payload[2], payload[3]]
      end
      # load rand stream directly - ensuring it doesn't get considered as a 'sample'
      rand_buf_id = server.buffer_alloc_read(buffers_path + "/rand-stream.wav").to_i
      old_samples = @samples
      @samples = {}

      (old_samples || {}).each do |k, v|
        message "Reloading sample - #{unify_tilde_dir(k)}"
        internal_load_sample(k, server)
      end

      old_synthdefs = @loaded_synthdefs
      @loaded_synthdefs = Set.new

      (old_synthdefs || []).each do |s|
        message "Reloading synthdefs in #{unify_tilde_dir(s)}"
        internal_load_synthdefs(s, server)
      end

      @recorders = {}
      @recording_mutex = Mutex.new
      @server = server
      @rand_buf_id = rand_buf_id
    end

    def error_occurred?
      @error_occured_mutex.synchronize do
        if @error_occurred_since_last_check
          @error_occurred_since_last_check = false
          return true
        else
          return false
        end
      end
    end

    def load_synthdefs(path, server=@server)
      check_for_server_rebooting!(:load_synthdefs)
      internal_load_synthdefs(path, server)
    end

    def sample_loaded?(path)
      return @samples.has_key?(path)
    end

    def load_sample(path, server=@server)
      check_for_server_rebooting!(:load_sample)
      internal_load_sample(path, server)
    end

    def free_sample(paths, server=@server)
      check_for_server_rebooting!(:free_sample)
      @sample_sem.synchronize do
        paths.each do |p|
          info = @samples[p]
          @samples.delete(p)
          server.buffer_free(info) if info
        end
      end
      :free
    end

    def free_all_samples(server=@server)
      check_for_server_rebooting!(:free_all_samples)
      @sample_sem.synchronize do
        @samples.each do |k, v|
          server.buffer_free(v)
        end
        @samples = {}
      end
    end


    def start_amp_monitor
      check_for_server_rebooting!(:start_amp_monitor)
      unless @amp_synth
        @amp_synth = @server.trigger_synth :head, @recording_group, "sonic-pi-amp_stereo_monitor", {"bus" => 0}, true
      end
    end


    def trigger_synth(synth_name, group, args, info, now=false, t_minus_delta=false )
      check_for_server_rebooting!(:trigger_synth)
      @server.trigger_synth(:head, group, synth_name, args, info, now, t_minus_delta)
    end

    def volume=(vol)
      check_for_server_rebooting!(:invert)
      message "Setting main volume to #{vol}"
      @server.node_ctl @mixer, {"amp" => vol}
    end

    def mixer_invert_stereo(invert)
      check_for_server_rebooting!(:mixer_invert_stereo)
      # invert should be true or false
      invert_i = invert ? 1 : 0
      @server.node_ctl @mixer, {"invert_stereo" => invert_i}, true
    end

    def mixer_control(opts)
      check_for_server_rebooting!(:mixer_control)
      now = 0
      opts = opts.clone
      if opts[:now].is_a?(Numeric)
        now = opts[:now]
      else
        now = opts[:now] ? 1 : 0
      end
      opts.delete :now
      @server.node_ctl @mixer, opts, now
    end

    def mixer_reset
      check_for_server_rebooting!(:mixer_reset)
      info = Synths::SynthInfo.get_info(:main_mixer)
      mixer_control(info.slide_arg_defaults)
      mixer_control(info.arg_defaults)
    end

    def mixer_stereo_mode
      check_for_server_rebooting!(:mixer_stereo_mode)
      @server.node_ctl @mixer, {"force_mono" => 0}, true
    end

    def mixer_mono_mode
      check_for_server_rebooting!(:mixer_mono_mode)
      @server.node_ctl @mixer, {"force_mono" => 1}, true
    end

    def status
      check_for_server_rebooting!(:status)
      @server.status
    end

    def stop
      check_for_server_rebooting!(:stop)
      @server.clear_schedule
      @server.group_clear @synth_group
    end

    def new_group(position, target, name="")
      check_for_server_rebooting!(:new_group)
      @server.create_group(position, target, name)
    end

    def new_synth_group(id=-1)
      check_for_server_rebooting!(:new_synth_group)
      new_group(:tail, @synth_group, "Run-#{id}-Synths")
    end

    def new_fx_group(id=-1)
      check_for_server_rebooting!(:new_fx_group)
      new_group(:tail, @fx_group, "Run-#{id}-FX")
    end

    def new_fx_bus
      check_for_server_rebooting!(:new_fx_bus)
      @server.allocate_audio_bus
    end

    def sched_ahead_time
      @server.sched_ahead_time
    end

    def sched_ahead_time=(t)
      @server.sched_ahead_time = t
    end

    def control_delta
      @server.control_delta
    end

    def control_delta=(t)
      @server.control_delta = t
    end

    def recording?(bus=0)
      @recorders[bus]
    end

    def recording_start(path, bus=0)
      check_for_server_rebooting!(:recording_start)
      return false if @recorders[bus]
      @recording_mutex.synchronize do
        return false if @recorders[bus]
        bs = @server.buffer_stream_open(path)
        s = @server.trigger_synth :head, @recording_group, "sonic-pi-recorder", {"out-buf" => bs.to_i, "in_bus" => bus.to_i}, true
        @recorders[bus] = [bs, s]
        true
      end
    end

    def recording_stop(bus=0)
      check_for_server_rebooting!(:recording_stop)
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

    def shutdown
      @server_reboot.kill
      begin
        @server.shutdown
      rescue Exception => e
      end
    end


    def reboot
      @reboot_queue.push :reboot
    end

    private

    def server_reboot
      # Important:
      # This method should only be called from the @server_rebooter
      # thread.
      @rebooting = true
      message "Rebooting server. Please wait..."
      @server.shutdown
      init_studio
      reset_server
      message "Server ready."
      @rebooting = false
      true
    end

    def check_for_server_rebooting!(msg=nil)
      if @rebooting
        message "Oops, already rebooting: #{msg}"
        log "Oops, already rebooting: #{msg}"
        raise StudioCurrentlyRebootingError if @rebooting
      end
    end

    def message(s)
      m = s.to_s
      log "Studio - #{m}"
      @msg_queue.push({:type => :info, :val => "Studio: #{m}"})
    end


    def reset_and_setup_groups_and_busses
      @server.clear_scsynth!
      @mixer_bus = @server.allocate_audio_bus
      @mixer_group = @server.create_group(:head, 0, "STUDIO-MIXER")
      @fx_group = @server.create_group(:before, @mixer_group, "STUDIO-FX")
      @synth_group = @server.create_group(:before, @fx_group, "STUDIO-SYNTHS")
      @recording_group = @server.create_group(:after, @mixer_group, "STUDIO-RECORDING")
    end

    def reset_server
      reset_and_setup_groups_and_busses
      start_mixer
    end

    def start_mixer
      #message "Starting mixer"
      # TODO create a way of swapping these on the fly:
      # set_mixer! :basic
      # set_mixer! :default
      mixer_synth = raspberry_pi_1? ? "sonic-pi-basic_mixer" : "sonic-pi-mixer"
      @mixer = @server.trigger_synth(:head, @mixer_group, mixer_synth, {"in_bus" => @mixer_bus.to_i}, nil, true)
    end

    def internal_load_sample(path, server=@server)
      return [@samples[path], true] if @samples[path]
      #message "Loading full sample path: #{path}"
      buf_info = nil
      @sample_sem.synchronize do
        return @samples[path] if @samples[path]
        raise "No sample exists with path:\n  #{unify_tilde_dir(path).inspect}" unless File.exists?(path) && !File.directory?(path)
          buf_info = server.buffer_alloc_read(path)
          buf_info.path = path
        @samples[path] = buf_info
      end
      [buf_info, false]
    end

    def internal_load_synthdefs(path, server=@server)
      @sample_sem.synchronize do
        server.load_synthdefs(path)
        @loaded_synthdefs << path
      end
    end

  end
end
