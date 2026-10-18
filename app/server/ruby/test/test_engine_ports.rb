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

require_relative "./setup_test"
require_relative "./lib/engine_ports"
require 'open3'
require 'rbconfig'

module SonicPi
  # Regression guarded against: the engine tests failed at random on CI,
  # "Address already in use" on Linux and "Permission denied" on Windows, as
  # one helper took a free TCP port from the system and hoped the UDP port of
  # that number was free too, and another checked only UDP for an engine that
  # binds TCP on the same number.
  class EnginePortsTester < Minitest::Test
    def setup
      @held = []
    end

    def teardown
      @held.each(&:close)
    end

    def test_a_claimed_port_binds_for_udp_and_for_tcp
      port = EnginePorts.claim
      hold(UDPSocket.new.tap { |s| s.bind("127.0.0.1", port) })
      hold(TCPServer.new("127.0.0.1", port))
    end

    def test_a_port_another_socket_holds_for_udp_is_not_free
      port = EnginePorts.claim
      assert EnginePorts.free?(port)
      hold(UDPSocket.new.tap { |s| s.bind("127.0.0.1", port) })
      refute EnginePorts.free?(port)
    end

    def test_a_port_another_socket_holds_for_tcp_is_not_free
      port = EnginePorts.claim
      assert EnginePorts.free?(port)
      hold(TCPServer.new("127.0.0.1", port))
      refute EnginePorts.free?(port)
    end

    def test_a_port_is_not_handed_out_twice_though_no_engine_has_bound_it_yet
      first = EnginePorts.claim
      refute_equal first, EnginePorts.claim
    end

    def test_a_span_is_not_handed_out_again_in_part
      span = EnginePorts.claim(2)
      refute_includes [span, span + 1], EnginePorts.claim
    end

    # No system hands these out to a client socket (Linux's ephemeral ports
    # start at 32768, macOS's and Windows' at 49152), and the daemon starts its
    # own search at 29152, so nothing else lands on one between the claim and
    # the engine's bind.
    def test_its_ports_are_below_every_ephemeral_range_and_the_daemons_search
      assert_operator EnginePorts.claim(2) + 1, :<, 29_152
    end

    # Two runs at once, each new, so neither has claimed anything before.
    def test_a_port_another_run_has_claimed_is_not_handed_out
      lib = File.expand_path("lib/engine_ports", __dir__)
      claim = "require #{lib.dump}; puts SonicPi::EnginePorts.claim; $stdout.flush"
      Open3.popen2(RbConfig.ruby, "-e", "#{claim}; $stdin.read") do |stdin, stdout, _|
        theirs = Integer(stdout.gets)
        mine, status = Open3.capture2(RbConfig.ruby, "-e", claim)
        assert status.success?
        refute_equal theirs, Integer(mine)
        stdin.close
      end
    end

    private

    def hold(socket)
      @held << socket
    end
  end
end
