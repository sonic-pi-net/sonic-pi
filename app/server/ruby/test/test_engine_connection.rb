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

# SuperSonic serves one client on its command port, so Spider's scsynth
# link and its MIDI, gamepad, Link and OSC comms share one connection.
# A fake engine here accepts connections and reads what arrives on each.

require 'minitest/autorun'
require 'socket'
require 'timeout'
require_relative '../lib/sonicpi/engine_connection'
require_relative '../lib/sonicpi/engine_secret'
require_relative '../lib/sonicpi/supersonic_comms'
require_relative '../lib/sonicpi/osc/oscdecode'

class EngineConnectionTest < Minitest::Test
  def setup
    @server = TCPServer.new('127.0.0.1', 0)
    @port = @server.addr[1]
    @decoder = SonicPi::OSC::OscDecode.new(false)
    @secret = SonicPi::EngineSecret.generate
    SonicPi::EngineConnection.reset!
    SonicPi::EngineConnection.secret = @secret
  end

  def teardown
    SonicPi::EngineConnection.reset!
    @server.close
  end

  def read_packet(sock)
    Timeout.timeout(5) do
      len = sock.read(4).unpack1("N")
      @decoder.decode_single_message(sock.read(len))
    end
  end

  def accept
    Timeout.timeout(5) { @server.accept }
  end

  def test_the_same_host_and_port_give_the_same_client
    a = SonicPi::EngineConnection.client('127.0.0.1', @port, name: 'a', connect_timeout: 5)
    b = SonicPi::EngineConnection.client('127.0.0.1', @port, name: 'b', connect_timeout: 5)
    assert_same a, b
    conn = accept
    assert_equal "/clockwork/auth", read_packet(conn)[0]
    assert_raises(Timeout::Error) { Timeout.timeout(0.3) { @server.accept } }   # one connection, not two
    conn.close
  end

  def test_two_comms_share_one_connection_and_both_hear_their_replies
    midi = SonicPi::SupersonicComms.new('127.0.0.1', @port, address_space: "/clockwork/midi", name: "midi")
    pads = SonicPi::SupersonicComms.new('127.0.0.1', @port, address_space: "/clockwork/gamepad", name: "pads")
    conn = accept
    assert_equal "/clockwork/auth", read_packet(conn)[0]
    midi.send("/clockwork/midi/ports")
    pads.send("/clockwork/gamepad/devices")
    assert_equal ["/clockwork/midi/ports", []], read_packet(conn)
    assert_equal ["/clockwork/gamepad/devices", []], read_packet(conn)

    heard = Queue.new
    midi.add_method("/clockwork/midi/ports.reply") { |args| heard << [:midi, args] }
    pads.add_method("/clockwork/gamepad/devices.reply") { |args| heard << [:pads, args] }
    enc = SonicPi::OSC::OscEncode.new(false)
    [["/clockwork/midi/ports.reply", ["in1"]], ["/clockwork/gamepad/devices.reply", ["pad1"]]].each do |addr, args|
      payload = enc.encode_single_message(addr, args)
      conn.write([payload.bytesize].pack("N") + payload)
    end
    got = [Timeout.timeout(5) { heard.pop }, Timeout.timeout(5) { heard.pop }].sort_by { |k, _| k.to_s }
    assert_equal [[:midi, ["in1"]], [:pads, ["pad1"]]], got
    conn.close
  end

  def test_every_reconnect_hook_runs_after_the_secret_is_presented_again
    client = SonicPi::EngineConnection.client('127.0.0.1', @port, name: 'c', connect_timeout: 5)
    ran = Queue.new
    client.on_reconnect { ran << :first }
    client.on_reconnect { ran << :second }
    first = accept
    assert_equal "/clockwork/auth", read_packet(first)[0]
    first.close
    second = accept
    assert_equal "/clockwork/auth", read_packet(second)[0]
    hooks = [Timeout.timeout(5) { ran.pop }, Timeout.timeout(5) { ran.pop }].sort
    assert_equal [:first, :second], hooks
    second.close
  end
end

class EngineConnectionListenerTest < Minitest::Test
  # A relay listens on an address the scsynth link already handles
  # (/clockwork/statechange): add_method keeps one handler per address, so a
  # listener is added beside it and both hear.
  def test_a_listener_hears_beside_the_address_handler
    server = TCPServer.new('127.0.0.1', 0)
    client = SonicPi::OSC::TcpOscClient.new('127.0.0.1', server.addr[1], name: 'l', connect_timeout: 5)
    conn = Timeout.timeout(5) { server.accept }
    heard = Queue.new
    client.add_method("/clockwork/statechange") { |args| heard << [:handler, args] }
    client.add_listener("/clockwork/statechange") { |args| heard << [:listener, args] }
    client.add_method("/clockwork/statechange") { |args| heard << [:handler2, args] }   # replaces the handler, not the listener
    enc = SonicPi::OSC::OscEncode.new(false)
    payload = enc.encode_single_message("/clockwork/statechange", ["running"])
    conn.write([payload.bytesize].pack("N") + payload)
    got = [Timeout.timeout(5) { heard.pop }, Timeout.timeout(5) { heard.pop }].sort_by { |k, _| k.to_s }
    assert_equal [[:handler2, ["running"]], [:listener, ["running"]]], got
    assert_raises(Timeout::Error) { Timeout.timeout(0.3) { heard.pop } }
  ensure
    client&.stop
    conn&.close
    server&.close
  end
end
