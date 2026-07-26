#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/osc/osc"
require 'timeout'

module SonicPi
  # Buffers travel GUI -> spider as a single UDP datagram, so the largest
  # runnable buffer is bounded by the OS datagram limit. Regressions guarded
  # against: the server truncating datagrams at a recvfrom cap smaller than
  # the UDP maximum, and macOS capping sends at net.inet.udp.maxdgram (9216
  # bytes) unless the socket raises SO_SNDBUF.
  class OSCUDPCapacityTester < Minitest::Test
    def with_udp_server
      server = nil
      attempts = 0
      begin
        port = 20_000 + rand(20_000)
        server = OSC::UDPServer.new(port, name: "udp capacity test")
      rescue Errno::EADDRINUSE
        attempts += 1
        retry if attempts < 10
        raise
      end
      yield server, port
    ensure
      server.stop if server
    end

    def test_64k_code_buffer_round_trip
      with_udp_server do |server, port|
        q = Queue.new
        server.add_method("/run-code") { |args| q.push(args) }

        code = "".dup
        code << "play 60 # udp capacity test padding\n" while code.bytesize < 65_000

        client = OSC::UDPClient.new("127.0.0.1", port)
        client.send("/run-code", 1234, code)

        args = Timeout.timeout(5) { q.pop }
        assert_equal 1234, args[0]
        assert_equal code, args[1]
      end
    end

    def test_oversize_datagram_raises
      with_udp_server do |_server, port|
        client = OSC::UDPClient.new("127.0.0.1", port)
        assert_raises(SystemCallError) do
          client.send("/run-code", 1234, "x" * 66_000)
        end
      end
    end
  end
end
