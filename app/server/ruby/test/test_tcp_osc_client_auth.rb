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

# SuperSonic closes a connection whose first packet is not /clockwork/auth
# with the per-boot secret as a 32-byte blob, and a reconnect is a new
# connection. A fake engine here reads what the client sends first.

require 'minitest/autorun'
require 'socket'
require 'timeout'
require_relative '../lib/sonicpi/osc/tcp_osc_client'
require_relative '../lib/sonicpi/osc/oscdecode'
require_relative '../lib/sonicpi/engine_secret'

class TcpOscClientAuthTest < Minitest::Test
  def setup
    @server = TCPServer.new('127.0.0.1', 0)
    @port = @server.addr[1]
    @decoder = SonicPi::OSC::OscDecode.new(false)
  end

  def teardown
    @client&.stop
    @server.close
  end

  # The next framed packet on a connection: its address and arguments.
  def read_packet(sock)
    Timeout.timeout(5) do
      len = sock.read(4).unpack1("N")
      @decoder.decode_single_message(sock.read(len))
    end
  end

  def test_the_first_packet_on_a_connection_is_the_secret
    secret = SonicPi::EngineSecret.generate
    @client = SonicPi::OSC::TcpOscClient.new('127.0.0.1', @port, name: 'auth-test',
                                             connect_timeout: 5, secret: secret)
    conn = Timeout.timeout(5) { @server.accept }
    @client.send(nil, nil, "/status")
    address, args = read_packet(conn)
    assert_equal "/clockwork/auth", address
    assert_equal 1, args.size
    assert_equal secret, args[0].to_s.b
    assert_equal ["/status", []], read_packet(conn)
    conn.close
  end

  def test_a_reconnect_presents_the_secret_again_before_anything_else
    secret = SonicPi::EngineSecret.generate
    @client = SonicPi::OSC::TcpOscClient.new('127.0.0.1', @port, name: 'auth-test',
                                             connect_timeout: 5, secret: secret)
    first = Timeout.timeout(5) { @server.accept }
    assert_equal "/clockwork/auth", read_packet(first)[0]
    first.close   # the engine went: the client reconnects on its own
    second = Timeout.timeout(5) { @server.accept }
    address, args = read_packet(second)
    assert_equal "/clockwork/auth", address
    assert_equal secret, args[0].to_s.b
    second.close
  end

  def test_without_a_secret_nothing_is_presented
    @client = SonicPi::OSC::TcpOscClient.new('127.0.0.1', @port, name: 'auth-test', connect_timeout: 5)
    conn = Timeout.timeout(5) { @server.accept }
    @client.send(nil, nil, "/status")
    assert_equal ["/status", []], read_packet(conn)
    conn.close
  end
end
