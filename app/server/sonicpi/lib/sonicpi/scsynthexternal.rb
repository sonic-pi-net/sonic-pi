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
require_relative "promise"

module SonicPi
  class SCSynthExternal
    include Util

    def initialize(opts={}, &callback)
      @hostname = opts[:hostname] || "localhost"
      @port = opts[:sc_port] || 4556
      @scsynth_pid = nil
      @jack_pid = nil
      @out_queue = SizedQueue.new(20)

      @client = OSC::Server.new(0)

      @client.add_method '*' do |m|
        callback.call(m.address, m.to_a)
      end

      @osc_in_thread = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_in)
        Thread.current.priority = -10
        log "starting server thread"
        @client.run
      end

      @osc_out_thread = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_out)
        loop do
          out_job = @out_queue.pop
          if out_job.first == :send
            @client.send(OSC::Message.new(*out_job[1]), @hostname, @port)
          else
            ts = out_job[1]
            args = out_job[2]
            m = OSC::Message.new(*args)
            b = OSC::Bundle.new(ts, m)
            @client.send(b, @hostname, @port)
          end
        end
      end

      boot
    end

    def send(*args)
      @out_queue << [:send, args]
    end

    def send_at(ts, *args)
      @out_queue << [:send_at, ts, args]
    end

    def reboot
      shutdown
      boot
    end

    def booted?
      !!@scsynth_pid
    end

    def shutdown
      if @jack_pid
        log "killing jack process"
        `kill #{@jack_pid}`
      end

      log "Sending /quit command to server"
      @client.send(OSC::Message.new("/quit"), @hostname, @port)
      @osc_in_thread.kill
      @osc_out_thread.kill
    end

    private

    def boot
      if booted?
        server_log "Server already booted..."
        return false
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
        # Do some globbing here for both 32/64 bit and different versions of SC
        "C:/Program Files (x86)/SuperCollider-3.6.6/scsynth.exe"
      end
    end

    def boot_and_wait(&boot_block)

      p = Promise.new
      connected = false

      boot_s = OSC::Server.new(5998)
      boot_s.add_method '*' do |m|
        p.deliver! true unless connected
        connected = true
      end

      t1 = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_external_booter)
        boot_s.run
      end

      t2 = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_external_boot_ack)
        loop do
          begin
            boot_s.send(OSC::Message.new("/status"), @hostname, @port)
          rescue Exception => e
          end
          sleep 0.25
        end
      end

      log "Starting the SuperCollider server..."
      yield

      begin
        p.get(10)
      rescue Exception => e
        boot_s.send(OSC::Message.new("/quit"), @hostname, @port)
      ensure
        t1.kill
        t2.kill
        boot_s.stop
      end

      raise "Unable to connect to scsynth" unless connected

    end

    def boot_server_osx
      log_boot_msg
      log "Booting on OS X"
      begin
        # NB OSX 10.6 system_profiler doesn't contain the necessary info for this command - will return 0
        osx_audio_settings = `system_profiler SPAudioDataType`.split("\n\n")
        osx_default_input = osx_audio_settings.select {|x| x[/Default Input Device: Yes/] }.first.to_s
        osx_default_output = osx_audio_settings.select {|x| x[/Default Output Device: Yes/] }.first.to_s
        osx_input_sample_rate = osx_default_input.match(/Current SampleRate: (\d+)/).captures.first
        osx_output_sample_rate = osx_default_output.match(/Current SampleRate: (\d+)/).captures.first

        if osx_output_sample_rate != 44100
          log "NOTICE: Non-standard sample rate detected. Booting SuperCollider with sample rate of #{osx_output_sample_rate} Hz"
        end

        if osx_input_sample_rate != osx_output_sample_rate
          # Let SuperCollider fix the sample rate using this one weird tip...
          # Send a command to start the server and let it fail with the 'input and output sample rates do not match' error
          # On the next boot it will have reset the sample rates and will start properly
          log "WARNING: input and output sample rates do not match. Trying to start SuperCollider again. See the following message:"
          sc_boot_msg = `'#{scsynth_path}' -u #{@port} -m 131072 -S #{osx_output_sample_rate} &`
          log sc_boot_msg
        end
      rescue
        log "WARNING: Sample rate could not be detected automatically. Please use the 'Audio MIDI Setup' to set the sample rate to 44100.0 Hz, otherwise SonicPi might not be able to start"
      end

      boot_and_wait do
        raise unless system("'#{scsynth_path}' -u #{@port} -m 131072 &")
      end
    end


    def boot_server_windows
      log_boot_msg
      log "Booting on Windows"
      boot_and_wait do
        system scsynth_path, "-u", @port.to_s
      end
    end

    def boot_server_raspberry_pi
      log_boot_msg
      log "Booting on Raspberry Pi"
      `killall jackd`
      `killall scsynth`
      system("jackd -R -T -p 32 -d alsa -n 3 -p 2048 -r 44100& ")

      # Wait for Jackd to start
      while `jack_wait -c`.match /not.*/
        sleep 0.25
      end

      @jack_pid = `ps cax | grep jackd`.split(" ").first

      boot_and_wait do
        system("scsynth -u #{@port} -m 131072 &")
      end

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
        system("jackd -R -T -p 32 -d alsa -n 3 -p 2048 -r 44100& ")

        # Wait for Jackd to start
        while `jack_wait -c`.match /not.*/
          sleep 0.25
        end
        @jack_pid = `ps cax | grep jackd`.split(" ").first
      else
        log "Jackd already running. Not starting another server..."
      end

      boot_and_wait do
        system("scsynth -u #{@port} -m 131072 &")
      end

      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`
    end
  end
end
