require 'singleton'
require_relative "util"

module SonicPi

  class SCSynth
    include Singleton
    include Util

    def initialize
      @port = 4556
      @scsynth_pid = nil
      boot
    end

    def boot
      if booted?
        server_log "Server already booted..."
        return false
      end

      case os
      when :linux
        boot_server_linux
      when :osx
        boot_server_osx
      when :windows
        raise "Windows is not yet supported..."
      end
      true
    end

    def reboot
      shutdown
      boot
    end

    def booted?
      !!@scsynth_pid
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
      when :linux
        "scsynth"
      when :osx
        osx_scsynth_path
      when :windows
        raise "Windows isn't yet supported, so no path here..."
      end
    end

    def shutdown
      if booted?
        `kill -9 #{@scsynth_pid}`
        log "Killed scsynth with PID #{@scsynth_pid}"
        @scsynth_pid = nil
        true
      else
        log "Unable to shutdown scsynth server - no pid found"
        false
      end
    end

    private

    def boot_server_osx
      log_boot_msg
      log "Booting on OS X"
      existing_scsynth_pids = `ps cax | grep scsynth`.split("\n").map{|l| l.split(" ").first}
      log "Starting the SuperCollider server..."
      system("#{scsynth_path} -u #{@port} -m 131072 &")
      sleep 4
      updated_scsynth_pids = `ps cax | grep scsynth`.split("\n").map{|l| l.split(" ").first}
      @scsynth_pid = (updated_scsynth_pids - existing_scsynth_pids).first
    end

    def boot_server_linux
      log_boot_msg
      log "Booting on Linux"
      #ensure dbus is running (for some reason jackd needs it)
      `eval $(dbus-launch --auto-syntax)`
      #Start Jack if not already running
      if `ps cax | grep jackd`.empty?
        #Jack not running - start a new instance and store its PID
        log "Jackd not running on system. Starting..."
        system("jackd -R -p 32 -d alsa -n 3& ")
        sleep 3
        jack_pid = `ps cax | grep jackd`.split(" ").first
        log "Jack started with pid #{jack_pid}"
        #write_jackd_pid(jack_pid)
      else
        log "Jackd already running. Not starting another server..."
      end

      #Start new instance of SuperCollider server and store its PID.
      existing_scsynth_pids = `ps cax | grep scsynth`.split("\n").map{|l| l.split(" ").first}
      log "Starting the SuperCollider server..."
      system("scsynth -u 4556 -m 131072 &")
      sleep 4
      updated_scsynth_pids = `ps cax | grep scsynth`.split("\n").map{|l| l.split(" ").first}
      @scsynth_pid = (updated_scsynth_pids - existing_scsynth_pids).first
      `jack_connect SuperCollider:out_1 system:playback_1`
      `jack_connect SuperCollider:out_2 system:playback_2`
    end
  end
end
