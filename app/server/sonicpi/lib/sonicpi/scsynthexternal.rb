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
require_relative "oscencode"
require_relative "scsynthoscreceiver"

module SonicPi
  class SCSynthExternal
    include Util

    def initialize(events, opts={})
      @hostname = opts[:hostname] || "localhost"
      @port = opts[:sc_port] || 4556
      @scsynth_pid = nil
      @jack_pid = nil
      @out_queue = SizedQueue.new(20)
      @client = ScsynthOSCReceiver.new(0, events)

      @osc_in_thread = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_in)
        Thread.current.priority = -10
        log "\n\n\n"
        log "Starting server thread"
        @client.run
      end

      @osc_out_thread = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_out)
        Thread.current.priority = 200
        encoder = OscEncode.new(true)
        loop do
          out_job = @out_queue.pop
          if out_job.first == :send
            address, *args = out_job[1]
            log "OSC             ~ #{address} #{args.inspect}" if osc_debug_mode
            m = encoder.encode_single_message(address, args)
            @client.send_raw(m, @hostname, @port)
          else
            vt = out_job[1]
            ts = out_job[2]
            address, *args = out_job[3]
            log "BDL #{'%11.5f' % vt} ~ [#{vt}:#{ts.to_i}] #{address} #{args.inspect}" if osc_debug_mode
            b = encoder.encode_single_bundle(ts, address, args)
            @client.send_raw(b, @hostname, @port)
          end
        end
      end
      boot
    end

    def sys(cmd)
      log "System: #{cmd}"
      system cmd
    end

    def send(*args)
      @out_queue << [:send,  args]
    end

    def send_at(ts, *args)
      if (a = Thread.current.thread_variable_get(:sonic_pi_spider_time)) && (b = Thread.current.thread_variable_get(:sonic_pi_spider_start_time))
        vt = a - b
      elsif st = Thread.current.thread_variable_get(:sonic_pi_spider_start_time)
        vt = ts - st
      else
        vt = -1
      end
      @out_queue << [:send_at, vt, ts, args]
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
      @client.send_raw(OSC::Message.new("/quit").encode, @hostname, @port)
      @osc_in_thread.kill
      @osc_out_thread.kill
    end

    private

    def boot
      if booted?
        server_log "Server already booted..."
        return false
      end
      log "booting server..."
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

    def boot_and_wait(&boot_block)
      p = Promise.new
      connected = false


      boot_s = OSC::Server.new(5998)
      boot_s.add_method '*' do |m|
        log "Boot - Receiving ack from server on port 5998"
        p.deliver! true unless connected
        connected = true
      end

      t1 = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_external_booter)
        boot_s.safe_run
      end

      t2 = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :scsynth_external_boot_ack)
        loop do
          begin
            log "Boot - Sending /status to server: #{@hostname}:#{@port}"
            boot_s.send(OSC::Message.new("/status"), @hostname, @port)
          rescue Exception => e
            log "Boot - Error sending /status to server: #{e.message}"
          end
          sleep 0.25
        end
      end

      log "Boot - Starting the SuperCollider server..."
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

      raise "Boot - Unable to connect to scsynth" unless connected

      log "Boot - Server connection established"
    end

    def boot_server_osx
      log_boot_msg
      log "Booting on OS X"
      boot_and_wait do
        sys("'#{scsynth_path}' -a #{num_audio_busses_for_current_os} -u #{@port} -m 131072 -D 0 &")
      end
    end


    def boot_server_windows
      log_boot_msg
      log "Booting on Windows"
      boot_and_wait do
        @scsynthpid = Process.spawn(scsynth_path, "-u", @port.to_s, "-a", num_audio_busses_for_current_os.to_s, "-m", "131072", "-D", "0")
        Process.detach(@scsynthpid)
      end
    end

    def boot_server_raspberry_pi
      log_boot_msg
      log "Booting on Raspberry Pi"
      `killall jackd`
      `killall scsynth`
      sys("jackd -R -p 32 -d alsa -n 3 -p 2048 -r 44100& ")

      # Wait for Jackd to start
      while `jack_wait -c`.match /not.*/
        sleep 0.25
      end

      @jack_pid = `ps cax | grep jackd`.split(" ").first

      buffer_size = raspberry_pi_1? ? 512 : 128

      boot_and_wait do
        sys("scsynth -u #{@port} -m 131072 -i 2 -o 2 -c 128 -a #{num_audio_busses_for_current_os} -z #{buffer_size} -D 0 -U /usr/lib/SuperCollider/plugins:#{native_path}/extra-ugens/ &")
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
        sys("jackd -R -T -p 32 -d alsa -n 3 -p 2048 -r 44100& ")

        # Wait for Jackd to start
        while `jack_wait -c`.match /not.*/
          sleep 0.25
        end
        @jack_pid = `ps cax | grep jackd`.split(" ").first
      else
        log "Jackd already running. Not starting another server..."
      end

      boot_and_wait do
        sys("scsynth -u #{@port} -m 131072 -a #{num_audio_busses_for_current_os} -D 0 &")
      end

      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`
    end
  end
end
