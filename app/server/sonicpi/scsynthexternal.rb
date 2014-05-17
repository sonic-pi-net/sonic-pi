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
require 'singleton'
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

      @client = OSC::Server.new(0)

      @client.add_method '*' do |m|
        callback.call(m.address, m.to_a)
      end

      @osc_thread = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :server_thread)
        Thread.current.priority = -10
        log "starting server thread"
        @client.run
      end

      boot

    end

    def send(*args)
      @client.send(OSC::Message.new(*args), @hostname, @port)
    end

    def send_at(ts, *args)
      m = OSC::Message.new(*args)
      b = OSC::Bundle.new(ts, m)
      @client.send(b, @hostname, @port)
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
      send "/quit"
      @osc_thread.kill
    end

    private

    def boot
      if booted?
        server_log "Server already booted..."
        return false
      end

      case os
      when :raspberry
        boot_server_linux
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
      potential_paths = ["/Applications/SuperCollider/scsynth",
        "/Applications/SuperCollider.app/Contents/Resources/scsynth",
        "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth"]
      path = potential_paths.find {|path| File.exists? path }
      raise "Unable to find SuperCollider. Is it installed?" unless path
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
        boot_s.run
      end

      t2 = Thread.new do
        loop do
          boot_s.send(OSC::Message.new("/status"), @hostname, @port)
          sleep 0.25
        end
      end

      log "Starting the SuperCollider server..."
      yield

      begin
        p.get_with_timeout(10, 0.2)
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
      boot_and_wait do
        system("#{scsynth_path} -u #{@port} -m 131072 &")
      end
    end


    def boot_server_windows
      log_boot_msg
      log "Booting on Windows"
      boot_and_wait do
        system scsynth_path, "-u", @port.to_s
      end
    end

    def boot_server_linux
      log_boot_msg
      log "Booting on Linux"
      #Start Jack if not already running
      if `ps cax | grep jackd`.empty?
        #Jack not running - start a new instance and store its PID
        log "Jackd not running on system. Starting..."
        system("jackd -R -p 32 -d alsa -n 3& ")
        raspberry? ? sleep(10) : sleep(3)
        @jack_pid = `ps cax | grep jackd`.split(" ").first
        log "Jack started with pid #{@jack_pid}"

      else
        log "Jackd already running. Not starting another server..."
      end

      boot_and_wait do
        system("scsynth -u #{@port} -m 131072 &")
      end

      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`

      sleep 3 if raspberry?
    end
  end
end
