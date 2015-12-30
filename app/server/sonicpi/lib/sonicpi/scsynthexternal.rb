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
require_relative "promise"
require_relative "osc/osc"
require 'fileutils'

module SonicPi
  class SCSynthExternal
    include Util

    def initialize(events, opts={})
      @events = events
      @hostname = opts[:hostname] || "localhost"
      @port = opts[:sc_port] || 4556
      @scsynth_pid = nil
      @jack_pid = nil
      @out_queue = SizedQueue.new(20)
      boot
    end

    def sys(cmd)
      log "System: #{cmd}"
      system cmd
    end

    def send(*all_args)
      address, *args = *all_args
      log "OSC             ~ #{address} #{args.inspect}" if osc_debug_mode
      @server.send(@hostname, @port, address, *args)
    end

    def send_at(ts, *all_args)
      address, *args = *all_args
      if osc_debug_mode
        if (a = Thread.current.thread_variable_get(:sonic_pi_spider_time)) && (b = Thread.current.thread_variable_get(:sonic_pi_spider_start_time))
          vt = a - b
        elsif st = Thread.current.thread_variable_get(:sonic_pi_spider_start_time)
          vt = ts - st
        else
          vt = -1
        end
        log "BDL #{'%11.5f' % vt} ~ [#{vt}:#{ts.to_i}] #{address} #{args.inspect}"
      end

      @server.send_ts(ts, @hostname, @port, address, *args)
    end

    def reboot
      shutdown
      boot
    end

    def booted?
      !!@scsynth_pid
    end

    def shutdown
      log "Sending /quit command to server"
      @server.send(@hostname, @port, "/quit")
      @server.stop
      t1, t2 = nil, nil
      if @jack_pid
        log "killing jack process #{@jack_pid}"
        t1 = Thread.new do
          kill_pid(@jack_pid)
          @jack_pid = nil
        end
      end

      if @scsynth_pid
        log "killing scynth process #{@scsynth_pid}"
        t2 = Thread.new do
          kill_pid(@scsynth_pid)
          @scsynth_pid = nil
        end

      end
      t1.join if t1
      t2.join if t2

      #close log file if it's open
      @scsynth_log_file.close if @scsynth_log_file
      @scsynth_log_file = nil
    end

    private

    def kill_pid_win(pid)
      # for some reason SIGINT doesn't seem to work
      # on Windows, so go straight for the jugular
      begin
        Process.kill(9, pid)
        log "Killed #{pid}"
        return true
      rescue Exception => e
        log "Could not kill #{pid} - perhaps already killed?"
        return nil
      end
    end

    def kill_pid(pid, safe_wait=4)
      log "going to kill #{pid}"
      return kill_pid_win(pid) if os == :windows
      pid = Integer(pid)
      Process.kill(15, pid)

      safe_wait.to_i.times do
        sleep 1
        begin
          alive = Process.waitpid(pid, Process::WNOHANG)
          return unless alive
        rescue SystemCallError
          # process is definitely dead!
          return nil
        end
      end

      Process.kill(9, pid)
    end

    def boot
      if booted?
        server_log "Server already booted..."
        return false
      end
      log "booting server..."

      @server = OSC::UDPServer.new(0, use_decoder_cache: true, use_encoder_cache: true)

      @server.add_global_method do |address, args|
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
        else
          @events.async_event address, args
        end
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
      log ""
      log ""
      log "Booting Sonic Pi"
      log "----------------"
      log ""
    end

    def osx_scsynth_path
      potential_paths = [
        "#{native_path}/scsynth",
        "/Applications/SuperCollider/scsynth",
        "/Applications/SuperCollider.app/Contents/Resources/scsynth",
        "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth"]
      path = potential_paths.find {|path| File.exists? path }
      raise "Unable to find SuperCollider. Is it installed? I looked here: #{potential_paths.inspect}" unless path
      path
    end

    def scsynth_path
      case os
      when :raspberry
        "scsynth"
      when :linux
        "scsynth"
      when :osx
        osx_scsynth_path
      when :windows
        potential_paths = ["#{native_path}/scsynth.exe"]
        path = potential_paths.find {|path| File.exists? path }
        raise "Unable to find SuperCollider. Is it installed? I looked here: #{potential_paths.inspect}" unless path
        path
      end
    end

    def boot_and_wait(*args)
      p = Promise.new
      p2 = Promise.new

      booted = false
      connected = false
      FileUtils.rm scsynth_log_path if File.exists?(scsynth_log_path)

      log "Boot - Starting the SuperCollider server..."
      @scsynth_log_file = File.open(scsynth_log_path, 'w')
      @scsynth_log_file.puts "# Starting SuperCollider #{Time.now.strftime("%Y-%m-%d %H:%M:%S")}"
      at_exit { @scsynth_log_file.close }
      scsynth_pipe = IO.popen(args)
      @scsynth_pid = scsynth_pipe.pid

      t1 = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_log_tracker)
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
        v = p.get(60)
      rescue Exception => e
        t1.kill
        Process.kill(9, @scsynth_pid)
      end
      raise "Unable to boot SuperCollider - boot server log does not report server ready" unless v

      boot_s = OSC::UDPServer.new(5998) do |a, b|
        log "Boot - Receiving ack from server on port 5998"
        p2.deliver! true unless connected
        connected = true
      end

      t2 = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_external_boot_ack)
        loop do
          begin
            log "Boot - Sending /status to server: #{@hostname}:#{@port}"
            boot_s.send(@hostname, @port, "/status")
          rescue Exception => e
            log "Boot - Error sending /status to server: #{e.message}"
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

      raise "Boot - Unable to connect to scsynth" unless connected

      log "Boot - Server connection established"
    end

    def boot_server_osx
      log_boot_msg
      log "Boot - Booting on OS X"
      log "Boot - Checkout audio rates on OSX:"
      # Force sample rate for both input and output to 44k
      # If these are not identical, then scsynth will refuse
      # to boot.
      begin
        audio_in_rate = :unknown_in_rate
        audio_out_rate = :unknown_out_rate
        require 'coreaudio'
        audio_in_rate = CoreAudio.default_input_device.nominal_rate
        audio_out_rate = CoreAudio.default_output_device.nominal_rate
        log "Boot - Input audio rate: #{audio_in_rate}"
        log "Boot - Output audio rate: #{audio_out_rate}"
        if audio_in_rate != audio_out_rate
          log "Attempting to set both in and out sample rates to 44100.0..."
          CoreAudio.default_output_device(nominal_rate: 44100.0)
          CoreAudio.default_input_device(nominal_rate: 44100.0)
          # now check again...
          audio_in_rate = CoreAudio.default_input_device.nominal_rate
          audio_out_rate = CoreAudio.default_output_device.nominal_rate
          log "Boot - Input audio rate now: #{audio_in_rate}"
          log "Boot - Output audio rate now: #{audio_out_rate}"
          if audio_in.nominal_rate != audio_out.nominal_rate
            log "Boot - Sample rates do not match, exiting"
            raise
          end
        else
          log "Boot - Sample rates match, we may continue to boot..."
        end

      rescue Exception => e
       raise "Unable to boot sound synthesis engine: the input and output rates of your audio card are not the same. Got in: #{audio_in_rate}, out: #{audio_out_rate}."
      end
      boot_and_wait(scsynth_path, "-u", @port.to_s, "-a", num_audio_busses_for_current_os.to_s, "-m", "131072", "-D", "0", "-R", "0", "-l", "1")
    end


    def boot_server_windows
      log_boot_msg
      log "Booting on Windows"

      boot_and_wait(scsynth_path, "-u", @port.to_s, "-a", num_audio_busses_for_current_os.to_s, "-m", "131072", "-D", "0", "-R", "0", "-l", "1")
    end

    def boot_server_raspberry_pi
      log_boot_msg
      log "Booting on Raspberry Pi"
      `killall jackd`
      `killall scsynth`
      begin
        asoundrc = File.read(Dir.home + "/.asoundrc")
        audio_card = (asoundrc.match /pcm.!default\s+{[^}]+\n\s+card\s+([0-9]+)/m)[1]
      rescue
        audio_card = "0"
      end
      sys("jackd -R -p 32 -d alsa -d hw:#{audio_card} -n 3 -p 2048 -r 44100& ")

      # Wait for Jackd to start
      while `jack_wait -c`.match /not.*/
        sleep 0.25
      end

      @jack_pid = `ps cax | grep jackd`.split(" ").first

      buffer_size = raspberry_pi_1? ? 512 : 128

      boot_and_wait("scsynth", "-u", @port.to_s, "-a", num_audio_busses_for_current_os.to_s, "-m", "131072", "-D", "0", "-R", "0", "-l", "1", "-z", buffer_size.to_s,  "-c", "128", "-U", "/usr/lib/SuperCollider/plugins:#{native_path}/extra-ugens/", "-i", "2", "-o", "2")

      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`

      sleep 3
    end

    def boot_server_linux
      log_boot_msg
      log "Booting on Linux"
      #Start Jack if not already running
      if `jack_wait -c`.match /not.*/
        #Jack not running - start a new instance
        log "Jackd not running on system. Starting..."
        sys("jackd -R -T -p 32 -d alsa -n 3 -p 2048 -r 44100& ")

        # Wait for Jackd to start
        while `jack_wait -c`.match /not.*/
          sleep 0.25
        end
        @jack_pid = `ps cax | grep jackd`.split(" ").first
      else
        log "Jackd already running. Not starting another server..."
      end

      boot_and_wait("scsynth", "-u", @port.to_s, "-m", "131072", "-a", num_audio_busses_for_current_os.to_s, "-D", "0", "-R", "0", "-l", "1")

      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`
    end
  end
end
