#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'fileutils'
require 'socket'
require 'tmpdir'

module SonicPi
  # Ports for the engines the tests start, which bind UDP and TCP on the same
  # number. Each is free for both when claimed, and nothing else takes it
  # before the engine binds it:
  #
  # - they lie below 29152, where the daemon starts its own search, and below
  #   every system's ephemeral ports (Linux's from 32768, macOS's and
  #   Windows' from 49152), so no client socket of any process lands on one;
  # - a claim holds a lock on each port's file for the rest of the run, so
  #   neither this run, whose last engine may not have bound yet, nor another
  #   run at the same time is handed it again. The system lets go of the lock
  #   when the run ends, however it ends.
  module EnginePorts
    PORTS = (24_000...29_152)
    LOCKS = File.join(Dir.tmpdir, "sonic-pi-test-ports")

    @claimed = {}
    @mutex = Mutex.new

    # The first of `span` consecutive ports, each free for UDP and TCP and
    # now claimed for the rest of the run.
    def self.claim(span = 1)
      @mutex.synchronize do
        FileUtils.mkdir_p(LOCKS)
        PORTS.each do |first|
          ports = (first...first + span)
          break unless PORTS.cover?(ports.last)
          locks = lock(ports)
          next unless locks
          ports.zip(locks).each { |p, l| @claimed[p] = l }
          return first
        end
        raise "no #{span} free ports from #{PORTS.first} to #{PORTS.last}"
      end
    end

    def self.free?(port)
      udp = UDPSocket.new
      udp.bind("127.0.0.1", port)
      TCPServer.new("127.0.0.1", port).close
      true
    rescue SystemCallError
      false
    ensure
      udp&.close
    end

    # Each port's lock, or nil (and none held) unless every one is taken.
    def self.lock(ports)
      locks = []
      ports.each do |p|
        return release(locks) if @claimed.key?(p)
        file = File.open(File.join(LOCKS, "#{p}.lock"), File::RDWR | File::CREAT)
        locks << file
        return release(locks) unless file.flock(File::LOCK_EX | File::LOCK_NB) && free?(p)
      end
      locks
    end
    private_class_method :lock

    def self.release(locks)
      locks.each(&:close)
      nil
    end
    private_class_method :release
  end
end
