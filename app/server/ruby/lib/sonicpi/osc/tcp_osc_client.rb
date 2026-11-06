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
require_relative "../util"
require_relative "oscencode"
require_relative "oscdecode"
require_relative "osc_types"
require_relative "../engine_secret"

module SonicPi
  module OSC
    # Framed OSC over a TCP connection to SuperSonic — the reliable
    # replacement for the UDP command port. Wire format matches the engine's
    # stream transports (TCP/UDS/named pipe): each OSC packet is prefixed
    # with its length as a 4-byte big-endian integer.
    #
    # Guarantees this transport adds over UDP:
    #   - no silent datagram loss (kernel backpressure instead of discard)
    #   - immediate death detection (connection reset, not timeout inference)
    #   - on Windows MRI, ~100x lower receive latency (the runtime's UDP
    #     receive path polls on a ~10ms timer; the TCP path blocks natively)
    #
    # Surface is deliberately UDPServer-compatible (send / send_ts /
    # add_method / add_global_method / stop) so call sites swap transports
    # without restructuring. The (address, port) arguments on send/send_ts
    # are accepted and ignored — the connection pins the destination.
    #
    # Sends are serialised with a mutex: concurrent writers to one stream
    # must not interleave frames (UDP got per-datagram atomicity for free).
    # Each message is a single write() of prefix+payload, so a frame never
    # straddles a partial segment under TCP_NODELAY.
    #
    # Reconnects: subscriptions/notify registrations at the engine are
    # per-connection, so after a reconnect the owning layer must
    # re-subscribe. Register an on_reconnect callback for that; each one
    # registered runs, in order, on a thread of its own once the connection
    # is re-established and the secret presented. The hooks wait for the
    # engine's answers, and the reader thread is what delivers them.
    class TcpOscClient
      attr_reader :encoder, :host, :port

      include Util

      # secret: the engine's per-boot secret (EngineSecret): presented as the
      # first packet on every connection, the reconnects included. Without
      # one nothing is presented, for a server that asks for none.
      # connect_timeout: total seconds to keep retrying the initial connect.
      # The engine binds its stream transport only after device init, which
      # can take >10s on Windows (ASIO device open) — be patient by default.
      def initialize(host, port, opts = {})
        @host = host
        @port = Integer(port)
        @name = opts[:name] || "Unnamed TCP OSC"
        @decoder = OscDecode.new(opts.fetch(:use_decoder_cache, true))
        @encoder = OscEncode.new(opts.fetch(:use_encoder_cache, true))
        @connect_timeout = opts.fetch(:connect_timeout, 60)
        @secret = opts[:secret]
        if @secret && @secret.bytesize != EngineSecret::BYTES
          raise ArgumentError, "#{@name}: a secret is #{EngineSecret::BYTES} bytes"
        end
        @matchers = {}
        @listeners = Hash.new { |h, k| h[k] = [] }
        @global_matcher = nil
        @send_mut = Mutex.new
        @on_reconnect = []
        @running = true
        @socket = connect_with_retry!(@connect_timeout)
        present_secret(@socket)
        @reader_thread = Thread.new { reader_loop }
        @reader_thread.name = "tcp-osc-reader: #{@name}"
      end

      def on_reconnect(&blk)
        @on_reconnect << blk
      end

      # UDPServer-compatible: (address, port) accepted and ignored — the
      # connection pins the destination.
      def send(_address, _port, pattern, *args)
        write_framed(@encoder.encode_single_message(pattern, args))
      end

      def send_ts(ts, _address, _port, pattern, *args)
        write_framed(@encoder.encode_single_bundle(ts, pattern, args))
      end

      # A HARNESS message for a moment in time. A timestamped bundle is the
      # DSP's: the engine parks it in scsynth's queue and the DSP answers
      # "Command not found" when a /clockwork/ address comes due there. The
      # harness's own scheduler takes "/clockwork/schedule <timetag> <blob>"
      # and re-ingests the inner message on its frame, through the same
      # dispatch an immediate one hits — this is how MidiAPI sends timed
      # MIDI, and how a track is played on time.
      def send_scheduled(ts, pattern, *args)
        inner = @encoder.encode_single_message(pattern, args)
        tt = SonicPi::OSC.osc_timetag(ts.to_f)
        write_framed(@encoder.encode_single_message("/clockwork/schedule",
                                                    [SonicPi::OSC::Int64.new(tt),
                                                     SonicPi::OSC::Blob.new(inner)]))
      end

      # An OSC packet already encoded, framed and sent as it is: a bundle of
      # several messages, which the encoder does not build.
      def send_packet(packet)
        write_framed(packet)
      end

      def add_method(address_pattern, &proc)
        @matchers[address_pattern] = proc
      end

      # Beside the address's handler, not in its place: a relay that passes
      # what arrives on to another process listens here, and the handler
      # the owning layer re-binds on each registration is untouched.
      def add_listener(address_pattern, &proc)
        @listeners[address_pattern] << proc
      end

      def add_global_method(&proc)
        @global_matcher = proc
      end

      def connected?
        !!(@socket && !@socket.closed?)
      end

      def to_s
        "#<SonicPi::OSC::TcpOscClient name: #{@name}, #{@host}:#{@port}>"
      end
      alias inspect to_s

      def stop
        @running = false
        begin
          @socket&.close
        rescue StandardError
        end
        @reader_thread&.kill
      end

      private

      def connect_with_retry!(timeout_s)
        deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout_s
        delay = 0.05
        begin
          sock = TCPSocket.new(@host, @port)
          sock.setsockopt(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)
          sock
        rescue StandardError => e
          if Process.clock_gettime(Process::CLOCK_MONOTONIC) >= deadline
            raise "#{@name}: could not connect to #{@host}:#{@port} within #{timeout_s}s (#{e.class}: #{e.message})"
          end
          sleep delay
          delay = [delay * 2, 1.0].min
          retry
        end
      end

      # Engine-side stream framing treats a frame beyond its MAX_FRAME
      # (256KB) as a protocol violation and severs the connection. Refuse
      # loudly here instead — a raised error names the oversized message; a
      # dropped connection would take every subsystem down with it.
      MAX_FRAME_BYTES = 256 * 1024

      # The secret, first on a new connection. Written straight to the socket:
      # the reconnect holds @send_mut, and nothing else may go first.
      def present_secret(sock)
        return unless @secret
        payload = @encoder.encode_single_message(EngineSecret::AUTH_ADDRESS, [SonicPi::OSC::Blob.new(@secret)])
        sock.write([payload.bytesize].pack("N") + payload)
      end

      def write_framed(payload)
        if payload.bytesize > MAX_FRAME_BYTES
          raise "#{@name}: OSC message too large for stream transport " \
                "(#{payload.bytesize} bytes > #{MAX_FRAME_BYTES})"
        end
        frame = [payload.bytesize].pack("N") + payload
        @send_mut.synchronize do
          raise "#{@name}: not connected" unless connected?
          @socket.write(frame)
        end
      end

      def read_exact(sock, n)
        buf = +""
        while buf.bytesize < n
          chunk = sock.read(n - buf.bytesize)
          raise EOFError, "connection closed" if chunk.nil?
          buf << chunk
        end
        buf
      end

      def reader_loop
        while @running
          begin
            sock = @socket
            hdr = read_exact(sock, 4)
            len = hdr.unpack1("N")
            # Defensive ceiling — matches the engine's own frame cap and
            # keeps a desynced/hostile stream from provoking a huge alloc.
            raise "oversize frame: #{len}" if len > 8 * 1024 * 1024
            packet = read_exact(sock, len)
            begin
              address, args = @decoder.decode_single_message(packet)
            rescue StandardError => e
              STDERR.puts "#{@name}: dropping undecodable frame (#{e.class}: #{e.message})"
              next
            end
            handle_data(address, args)
          rescue Exception => e
            break unless @running
            STDERR.puts "#{@name}: connection lost (#{e.class}: #{e.message}) - reconnecting"
            begin
              @send_mut.synchronize do
                begin
                  @socket&.close
                rescue StandardError
                end
                @socket = connect_with_retry!(@connect_timeout)
                present_secret(@socket)
              end
              STDERR.puts "#{@name}: reconnected"
              run_reconnect_hooks
            rescue Exception => e2
              break unless @running
              STDERR.puts "#{@name}: reconnect failed permanently (#{e2.message})"
              break
            end
          end
        end
      end

      # The hooks register with the engine again and wait for its answers,
      # which only the reader thread delivers: they run on a thread of their
      # own, in order, and the reader goes back to reading.
      def run_reconnect_hooks
        hooks = @on_reconnect.dup
        t = Thread.new do
          hooks.each do |blk|
            begin
              blk.call
            rescue StandardError => e
              STDERR.puts "#{@name}: reconnect hook failed (#{e.class}: #{e.message})"
            end
          end
        end
        t.name = "tcp-osc-reconnect: #{@name}"
      end

      # The owner's handler, the listeners and the global handler all hear
      # the message. One that fails is its own problem: the rest still run.
      def handle_data(address, args)
        log "TCP OSC <----- #{address} #{args.inspect}" if incoming_osc_debug_mode
        if (p = @matchers[address])
          guarded(address, "handler") { p.call(args) }
        end
        @listeners[address].each { |l| guarded(address, "listener") { l.call(args) } } if @listeners.key?(address)
        guarded(address, "global handler") { @global_matcher&.call(address, args, nil) }
      end

      def guarded(address, what)
        yield
      rescue StandardError => e
        STDERR.puts "#{@name}: #{what} for #{address} failed (#{e.class}: #{e.message})"
      end
    end
  end
end
