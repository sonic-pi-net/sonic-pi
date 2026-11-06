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
require 'timeout'
require_relative "../../lib/sonicpi/osc/oscencode"
require_relative "../../lib/sonicpi/osc/oscdecode"

module SonicPi
  # The engine's command port, faked: a TCP server speaking the engine's
  # framing (a 4-byte big-endian length, then one OSC message) that records
  # what each connection sends and answers by address as a test tells it
  # to. For what the real engine does, the headless engine tests are the
  # ones to read; this is for the client's own behaviour around a
  # connection: what it does when one drops, and what it does with what
  # arrives.
  class FakeEngine
    attr_reader :port, :received

    def initialize
      @server = TCPServer.new('127.0.0.1', 0)
      @port = @server.addr[1]
      @encoder = OSC::OscEncode.new(false)
      @decoder = OSC::OscDecode.new(false)
      @replies = {}
      @received = Queue.new      # [connection number, address, args]
      @connections = []
      @accepted = Queue.new
      @mutex = Mutex.new
      @acceptor = Thread.new { accept_loop }
    end

    # Answer `address` on the connection it came in on with what the block
    # returns: a message ([address, *args]), several ([[address, *args], ...])
    # or nil for nothing.
    def on(address, &blk)
      @replies[address] = blk
    end

    # The next connection the client makes, within `timeout` seconds: its
    # number, from 1.
    def await_connection(timeout = 5)
      Timeout.timeout(timeout) { @accepted.pop }
    end

    # The next message the client sent, within `timeout` seconds:
    # [connection number, address, args].
    def next_received(timeout = 5)
      Timeout.timeout(timeout) { @received.pop }
    end

    # The engine went: the client's connection is closed from this side.
    def drop(number)
      conn = @mutex.synchronize { @connections[number - 1] }
      conn&.close rescue nil
    end

    # A message from the engine on connection `number`.
    def send(number, address, *args)
      conn = @mutex.synchronize { @connections[number - 1] }
      conn.write(frame(address, args))
    end

    def stop
      @server.close rescue nil
      @mutex.synchronize { @connections.each { |c| c.close rescue nil } }
      @acceptor.join(1)
    end

    private

    def frame(address, args)
      payload = @encoder.encode_single_message(address, args)
      [payload.bytesize].pack("N") + payload
    end

    def accept_loop
      loop do
        conn = @server.accept
        conn.setsockopt(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)
        number = @mutex.synchronize { @connections << conn; @connections.size }
        Thread.new { serve(conn, number) }
        @accepted << number
      end
    rescue IOError, SystemCallError
      nil
    end

    def serve(conn, number)
      loop do
        header = conn.read(4)
        break unless header && header.bytesize == 4
        packet = conn.read(header.unpack1("N"))
        break unless packet
        address, args = @decoder.decode_single_message(packet)
        @received << [number, address, args]
        replies = @replies[address]&.call(args)
        replies = [replies] if replies && !replies[0].is_a?(Array)
        replies&.each { |reply| conn.write(frame(reply[0], reply[1..])) }
      end
    rescue IOError, SystemCallError
      nil
    ensure
      conn.close rescue nil
    end
  end
end
