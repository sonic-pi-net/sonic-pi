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

require 'socket'
require 'tmpdir'
require 'timeout'
require_relative "../mix/lib/mix_engine"

module SonicPi
  # A headless SuperSonic for a test that needs the real engine: its own port
  # (UDP and TCP, the same number, as Sonic Pi starts it), no audio device, and
  # its log kept so a failing test can say what the engine saw. The binary is
  # the one the mix tests find (MixEngine::ENGINE).
  class HeadlessEngine
    attr_reader :port, :log

    def self.available?
      File.executable?(MixEngine::ENGINE)
    end

    def initialize(name)
      @port = free_port
      @log = File.join(Dir.tmpdir, "sonic-pi-#{name}-#{@port}.log")
      @pid = Process.spawn(MixEngine::ENGINE, "--headless", "-u", @port.to_s, "--tcp", @port.to_s,
                           "-o", "2", "-i", "0", "-a", "1024", "-b", "4096", "-B", "127.0.0.1",
                           out: @log, err: [:child, :out])
    end

    def log_tail(lines = 80)
      File.exist?(@log) ? File.read(@log).lines.last(lines).join : ""
    end

    def stop
      return unless @pid
      Process.kill("TERM", @pid) rescue nil
      begin
        Timeout.timeout(5) { Process.wait(@pid) }
      rescue Timeout::Error
        Process.kill("KILL", @pid) rescue nil
        Process.wait(@pid) rescue nil
      end
      @pid = nil
    end

    # What the server's Studio takes for its system state: only the
    # sched-ahead time is asked for.
    class State
      def sched_ahead_time_at(_t)
        0.5
      end
    end

    private

    def free_port
      tcp = TCPServer.new("127.0.0.1", 0)
      port = tcp.addr[1]
      tcp.close
      udp = UDPSocket.new
      udp.bind("127.0.0.1", port)
      udp.close
      port
    end
  end
end
