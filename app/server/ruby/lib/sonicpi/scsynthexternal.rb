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
require_relative "promise"
require_relative "osc/osc"
require_relative "thread_id"

require 'fileutils'


module SonicPi
  class SCSynthExternal
    include Util

    attr_reader :version

    def initialize(events, opts={})
      @events = events
      @hostname = opts[:hostname] || "127.0.0.1"
      @port = opts[:scsynth_port] || 4556
      @send_port = opts[:scsynth_send_port] || 4556
      @register_cue_event_lambda = opts[:register_cue_event_lambda]
      raise "No cue event lambda!" unless @register_cue_event_lambda
      @out_queue = SizedQueue.new(20)
      @scsynth_thread_id = ThreadId.new(-5)
      @version = request_version.freeze
      boot
    end

    def sys(cmd)
      log "System: #{cmd}"
      system cmd
    end

    def send(*all_args)
      address, *args = *all_args
      log "OSC             ~ #{address} #{args.inspect}" if osc_debug_mode
      @osc_server.send(@hostname, @send_port, address, *args)
    end

    def send_at(ts, *all_args)
      address, *args = *all_args
      if osc_debug_mode

        if (a = __system_thread_locals.get(:sonic_pi_spider_time)) && (b = __system_thread_locals.get(:sonic_pi_spider_start_time))
          vt = a - b
        elsif st = __system_thread_locals.get(:sonic_pi_spider_start_time)
          vt = ts - st
        else
          vt = -1
        end
        log "BDL #{'%11.5f' % vt} ~ [#{vt}:#{ts.to_f}] #{address} #{args.inspect}"
      end

      @osc_server.send_ts(ts, @hostname, @send_port, address, *args)
    end

    def reboot
      shutdown
      boot
    end

    def booted?
      !!@scsynth_pid
    end

    def shutdown
      puts "Sending /quit command to scsynth"
      begin
        @osc_server.send(@hostname, @send_port, "/quit")
      rescue Exception => e
        puts "Error during scsynth shutdown when attempting to send /quit OSC message to server #{@hostname} on port #{@send_port}"
        puts " --> #{e.message}"
        puts " --> #{e.backtrace.inspect}\n\n"
      end
      puts "Stopping OSC server..."
      @osc_server.stop
      puts "Stopped OSC server..."
      t1, t2 = nil, nil
      t1.join if t1
      t2.join if t2

      #close log file if it's open
      @scsynth_log_file.close if @scsynth_log_file
      @scsynth_log_file = nil
    end

    private

    def request_version
      version_string = `"#{scsynth_path}" -v`
      m = version_string.match /\A\s*scsynth\s+([0-9.a-zA-Z-]+)\s.*/
      if m && m[1] && !m[1].empty?
        "v#{m[1]}"
      else
        ""
      end
    end

    def boot
      if booted?
        server_log "Server already booted..."
        return false
      end
      puts "Booting server..."

      @osc_server = OSC::UDPServer.new(0, use_decoder_cache: true, use_encoder_cache: true)

      @osc_server.add_global_method do |address, args, info|
        case address
        when "/n_end"
          id = args[0].to_i
          @events.async_event "/n_end/#{id}", args
        when "/n_off"
          id = args[0].to_i
          @events.async_event "/n_off/#{id}", args
        when "/n_on"
          id = args[0].to_i
          @events.async_event "/n_on/#{id}", args
        when "/n_go"
          id = args[0].to_i
          @events.async_event "/n_go/#{id}", args
        when "/n_move"
          id = args[0].to_i
          @events.async_event "/n_move/#{id}", args
        else
          @events.async_event address, args
        end
        p = 0
        d = 0
        b = 0
        m = 60
        @register_cue_event_lambda.call(Time.now, p, @scsynth_thread_id, d, b, m, address, args) if address.start_with? "/scsynth/"
      end


      case os
      when :raspberry
        boot_server_raspberry_pi
      when :linux
        boot_server_linux
      when :osx
        boot_server_osx
      when :windows
        boot_server_windows
      end
      true
    end

    def raspberry?
      os == :raspberry
    end

    def log_boot_msg
      puts ""
      puts ""
      puts "Booting Sonic Pi"
      puts "----------------"
      puts ""
      log "\n\n\n"
    end

    def scsynth_path
      case os
      when :raspberry
        "scsynth"
      when :linux
        "scsynth"
      when :osx
        path = "#{native_path}/scsynth"
        raise "Unable to find SuperCollider. Is it installed? I looked here: #{path.inspect}" unless File.exist?(path)
        path
      when :windows
        path = "#{native_path}/scsynth.exe"
        raise "Unable to find SuperCollider. Is it installed? I looked here: #{path.inspect}" unless File.exist?(path)
        path
      end
    end

    def boot_and_wait(*args)
      puts "Boot - Starting the SuperCollider server..."
      puts "Boot - #{args.join(' ')}"
      p = Promise.new
      p2 = Promise.new

      booted = false
      connected = false
      begin
        FileUtils.rm scsynth_log_path if File.exist?(scsynth_log_path)
        @scsynth_log_file = File.open(scsynth_log_path, 'w')
      rescue
        @scsynth_log_file = nil
      end
      @scsynth_log_file.puts "# Starting SuperCollider #{Time.now.strftime("%Y-%m-%d %H:%M:%S")}" if @scsynth_log_file
      at_exit { @scsynth_log_file.close if @scsynth_log_file}
      scsynth_pipe = IO.popen(args)
      @scsynth_pid = scsynth_pipe.pid
      register_process(@scsynth_pid)
      t1 = Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, :scsynth_log_tracker)
        scsynth_pipe.each_line do |l|
          @scsynth_log_file.puts l if @scsynth_log_file
          @scsynth_log_file.flush if @scsynth_log_file
          if !booted && l =~ /SuperCollider 3 server ready/
            p.deliver! true
            booted = true
          end
        end
      end

      begin
        p.get(60)
      rescue PromiseTimeoutError => e
        kill_and_deregister_process(@scsynth_id)
        t1.kill
        msg = "Boot - Unable to boot SuperCollider - boot server log did not report server ready"
        puts msg
        raise msg
      end

      puts "Boot - SuperCollider booted successfully."
      puts "Boot - Connecting to the SuperCollider server..."

      boot_s = OSC::UDPServer.new(0) do |a, b, info|
        puts "Boot - Receiving ack from scsynth"
        p2.deliver! true unless connected
        connected = true
      end

      t2 = Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, :scsynth_external_boot_ack)
        Kernel.loop do
          begin
            puts "Boot - Sending /status to server: #{@hostname}:#{@send_port}"
            boot_s.send(@hostname, @send_port, "/status")
          rescue Exception => e
            puts "Boot - Error sending /status to server: #{e.message}"
          end
          sleep 2
        end
      end

      begin
        p2.get(30)
      rescue Exception => e
        Process.kill(9, @scsynth_pid)
      ensure
        t2.kill
        boot_s.stop
      end

      unless connected
        puts "Boot - Unable to connect to SuperCollider"
        raise "Boot - Unable to connect to SuperCollider"
      end

      puts "Boot - Server connection established"
    end

    def boot_server_osx
      disable_input = true
      log_boot_msg
      puts "Boot - Booting on OS X"
      puts "Boot - Checkout audio rates on OSX:"
      # Force sample rate for both input and output to 44k
      # If these are not identical, then scsynth will refuse
      # to boot.
      begin
        audio_in_rate = :unknown_in_rate
        audio_out_rate = :unknown_out_rate
        require 'coreaudio'
        audio_in_rate = CoreAudio.default_input_device.nominal_rate
        audio_out_rate = CoreAudio.default_output_device.nominal_rate
        puts "Boot - Input audio rate: #{audio_in_rate}"
        puts "Boot - Output audio rate: #{audio_out_rate}"
        if (audio_in_rate != :unknown_in_rate) && (audio_out_rate != :unknown_out_rate) && (audio_in_rate != audio_out_rate)
          # we detected audio rates, but they're not the same
          puts "Boot - Audio input and output rates do not match."
          if audio_out_rate > 44000
            puts "Boot - Attempting to set the input rates to match output rate of #{audio_out_rate}..."
            CoreAudio.default_input_device(nominal_rate: audio_out_rate)
          end

          audio_in_rate = CoreAudio.default_input_device.nominal_rate
          audio_out_rate = CoreAudio.default_output_device.nominal_rate

          if (audio_in_rate != :unknown_in_rate) && (audio_out_rate != :unknown_out_rate) && (audio_in_rate != audio_out_rate)
            # we detected audio rates, and they're still not the same
            puts "Boot - Attempting to set both in and out sample rates to 44100.0..."
            CoreAudio.default_output_device(nominal_rate: 44100.0)
            CoreAudio.default_input_device(nominal_rate: 44100.0)
          end

          puts "Boot - Input audio rate now: #{audio_in_rate}"
          puts "Boot - Output audio rate now: #{audio_out_rate}"
          if (audio_in_rate != :unknown_in_rate) && (audio_out_rate != :unknown_out_rate) && (audio_in_rate != audio_out_rate)
            puts "Boot - Sample rates still do not match, disabling input"
          else
            puts "Boot - Sample have been changed to match, enable input"
            disable_input = false
          end
        else
          puts "Boot - Sample rates match, enable support for inputs..."
          disable_input = false
        end

      rescue Exception
        if (audio_in_rate == :unknown_in_rate) || (audio_out_rate == :unknown_out_rate)
          # Something went wrong whilst attempting to determine and modify the audio
          # rates. For safety do not enable inputs
          puts "Boot - Unable to detect audio rates. Disabling input"
        end
      end

      if disable_input
        num_inputs = "0"
        puts "Boot - Booting with no audio inputs"
      else
        num_inputs = "16"
        puts "Boot - Booting with max 16 inputs"
      end

      boot_and_wait(scsynth_path,
                    "-u", @port.to_s,
                    "-a", num_audio_busses_for_current_os.to_s,
                    "-m", "131072",
                    "-D", "0",
                    "-R", "0",
                    "-l", "1",
                    "-i", num_inputs,
                    "-o", "16",
                    "-U", "#{native_path}/supercollider/plugins/",
                    "-b", num_buffers_for_current_os.to_s,
                    "-B", "127.0.0.1")
    end


    def boot_server_windows
      log_boot_msg
      puts "Booting on Windows"

      boot_and_wait(scsynth_path,
                    "-u", @port.to_s,
                    "-m", "131072",
                    "-a", num_audio_busses_for_current_os.to_s,
                    "-D", "0",
                    "-R", "0",
                    "-l", "1",
                    "-i", "16",
                    "-o", "16",
                    "-U", "#{native_path}/plugins/",
                    "-b", num_buffers_for_current_os.to_s,
                    "-B", "127.0.0.1")
    end

    def boot_server_raspberry_pi
      log_boot_msg
      puts "Booting on Raspberry Pi"
      begin
        asoundrc = File.read(Dir.home + "/.asoundrc")
        audio_card = (asoundrc.match(/pcm.!default\s+{[^}]+\n\s+card\s+([0-9]+)/m))[1]
      rescue
        audio_card = "0"
      end

      #Start Jack if not already running
      if `ps cax | grep jackd`.split(" ").first.nil?
        #Jack not running - start a new instance
        puts "Jackd not running on system. Starting..."
        jack_pid = spawn "jackd -R -p 32 -d alsa -d hw:#{audio_card} -n 3 -p 2048 -o2 -r 44100& "
        register_process jack_pid
      else
        puts "Jackd already running. Not starting another server..."
      end

      register_process jack_pid
      block_size = raspberry_pi_1? ? 512 : 128

      boot_and_wait("scsynth",
                    "-u", @port.to_s,
                    "-m", "131072",
                    "-a", num_audio_busses_for_current_os.to_s,
                    "-D", "0",
                    "-R", "0",
                    "-l", "1",
                    "-i", "2",
                    "-o", "2",
                    "-z", block_size.to_s,
                    "-c", "128",
                    "-U", "/usr/lib/SuperCollider/plugins:#{native_path}/extra-ugens/",
                    "-b", num_buffers_for_current_os.to_s,
                    "-B", "127.0.0.1")

      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`
      # `jack_connect SuperCollider:in_1 system_capture_1`
      # `jack_connect SuperCollider:in_2 system_capture_2`

      sleep 3
    end

    def boot_server_linux
      log_boot_msg
      puts "Booting on Linux"
      #Start Jack if not already running
      if `ps cax | grep jackd`.split(" ").first.nil?
        #Jack not running - start a new instance
        puts "Jackd not running on system. Starting..."
        jack_pid = ("jackd -R -T -p 32 -d alsa -n 3 -p 2048 -r 44100& ")
        register_process jack_pid
      else
        puts "Jackd already running. Not starting another server..."
      end

      boot_and_wait("scsynth",
                    "-u", @port.to_s,
                    "-m", "131072",
                    "-a", num_audio_busses_for_current_os.to_s,
                    "-D", "0",
                    "-R", "0",
                    "-l", "1",
                    "-i", "16",
                    "-o", "16",
                    "-b", num_buffers_for_current_os.to_s,
                    "-B", "127.0.0.1")

      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`
      `jack_connect SuperCollider:in_1 system:capture_1`
      `jack_connect SuperCollider:in_2 system:capture_2`
    end
  end
end
