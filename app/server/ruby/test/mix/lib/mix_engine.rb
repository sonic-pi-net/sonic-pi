#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'fileutils'
require 'tmpdir'

require_relative "../../../lib/sonicpi/osc/osc"

module SonicPi
  # Renders audio through a real mixer synthdef in a headless SuperSonic and
  # captures the result exactly, so mix changes can be measured rather than
  # argued about. No audio device is involved: the engine runs on its own
  # timer and the capture is a RecordBuf written straight to disk.
  class MixEngine
    SAMPLE_RATE = 48_000

    ROOT = File.expand_path("../../../../../..", __dir__)

    # The shipping engine by default, with a `.exe` fallback for Windows.
    # SONIC_PI_MIX_ENGINE overrides it, so a candidate build carrying new
    # UGens can be measured without overwriting the installed binary.
    ENGINE = ENV.fetch("SONIC_PI_MIX_ENGINE") do
      [File.join(ROOT, "app/server/native/sonic-pi-supersonic"),
       File.join(ROOT, "app/server/native/Sonic Pi - SuperSonic"),
       File.join(ROOT, "app/server/native/Sonic Pi - SuperSonic.exe")]
        .find { |p| File.executable?(p) } ||
        File.join(ROOT, "app/server/native/sonic-pi-supersonic")
    end
    SYNTHDEFS = File.join(ROOT, "etc/synthdefs/compiled")
    HARNESS_DEFS = File.expand_path("../synthdefs/compiled", __dir__)

    # Private audio bus the test source plays into; the mixer reads it and
    # replaces bus 0, which is where the capture listens.
    SOURCE_BUS = 16.0
    SOURCE_NODE = 1000
    MIXER_NODE = 1001
    CAPTURE_NODE = 1002
    SOURCE_BUF = 1
    CAPTURE_BUF = 2

    # The mixer's pre_amp and amp are lagged by 20 ms and its safety filters
    # need a moment to settle. Everything before this is startup transient,
    # not signal, and measuring it reports distortion that isn't there.
    #
    # This has to clear the SLOWEST time constant in the chain, which is the
    # limiter's release: SPLimiter's is program-dependent and runs to ~0.4 s
    # at shallow gain reduction. A window that starts before the gain has
    # settled catches it still creeping, which smears energy either side of
    # the test tone and reads as THD+N: a just-over-ceiling 1 kHz sine
    # measures -46 dB from 250 ms and -143 dB from 2 s, on identical audio.
    SETTLE_MS = 1500

    class EngineError < StandardError; end

    Capture = Struct.new(:l, :r, :sample_rate) do
      # The measurable part of a render: settle window removed from the head,
      # and the tail trimmed to whole blocks.
      def steady(settle_ms = SETTLE_MS)
        skip = (sample_rate * settle_ms / 1000.0).to_i
        Capture.new(l[skip..] || [], r[skip..] || [], sample_rate)
      end

      def channels
        [l, r]
      end

      def frames
        l.size
      end
    end

    attr_reader :port

    def initialize(port: 57_190, log: nil)
      @port = port
      @log_path = log || File.join(Dir.tmpdir, "sonic-pi-mix-harness-#{port}.log")
      @replies = Queue.new
      @loaded_mixer = nil
    end

    def start
      raise EngineError, "engine not built: #{ENGINE}" unless File.executable?(ENGINE)
      # An engine left over from an aborted run still holds the port. The new
      # one boots, silently fails to bind, and then every await times out
      # with nothing to suggest why, so check first and say so plainly.
      check_port_free
      @pid = Process.spawn(ENGINE, "--headless", "-u", @port.to_s, "-o", "2", "-i", "0",
                           out: @log_path, err: [:child, :out])
      @server = OSC::UDPServer.new(@port + 1) { |addr, *args| @replies << [addr, args] }
      wait_for_boot
      enable_notifications
      load_synthdefs
      self
    end

    def stop
      return unless @pid
      send_osc("/quit")
      begin
        Timeout.timeout(3) { Process.wait(@pid) }
      rescue StandardError
        Process.kill("KILL", @pid) rescue nil
        Process.wait(@pid) rescue nil
      end
      @pid = nil
    end

    # Loads a mixer synthdef by path. Defaults to the shipping one; pass a
    # candidate build to A/B a proposed chain against it.
    def load_mixer(path = File.join(SYNTHDEFS, "sonic-pi-mixer.scsyndef"))
      raise EngineError, "no such synthdef: #{path}" unless File.exist?(path)
      send_osc("/d_load", path)
      await("/done")
      @loaded_mixer = path
    end

    # Plays `wav_path` through the mixer with `args` (control name => value)
    # and returns the captured output. Control values are coerced to Float:
    # scsynth takes an integer where a float is expected as a different
    # value entirely, which silently renders silence.
    def render(wav_path, duration, args = {})
      load_mixer unless @loaded_mixer
      frames = (duration * SAMPLE_RATE).to_i

      # Replies are matched positionally against a shared queue, so anything
      # still sitting in it from the previous render shifts every await that
      # follows and the next one to expect a /done waits for a reply that
      # already came and went. Start each render from an empty queue.
      drain_replies

      send_osc("/b_free", SOURCE_BUF)
      send_osc("/b_free", CAPTURE_BUF)
      await("/done"); await("/done")
      send_osc("/b_allocRead", SOURCE_BUF, wav_path, 0, 0)
      send_osc("/b_alloc", CAPTURE_BUF, frames, 2)
      await("/done")
      confirm_buffer(SOURCE_BUF)

      mixer_args = { "in_bus" => SOURCE_BUS, "out_bus" => 0.0 }.merge(
        args.transform_keys(&:to_s).transform_values { |v| Float(v) })

      # One bundle, so all three nodes start in the same control block. Sent
      # as separate messages they can straddle a block boundary differently
      # from render to render, and two captures that should be identical end
      # up offset by up to a block, which makes any A/B comparison between
      # renders meaningless.
      #
      # Tail first, then each earlier stage at the head, so the render order
      # ends up source -> mixer -> capture.
      send_bundle(
        ["/s_new", "mixtest-capture", CAPTURE_NODE, 1, 0,
         "buf", CAPTURE_BUF.to_f, "in_bus", 0.0],
        ["/s_new", "sonic-pi-mixer", MIXER_NODE, 0, 0, *mixer_args.to_a.flatten],
        ["/s_new", "mixtest-source", SOURCE_NODE, 0, 0,
         "buf", SOURCE_BUF.to_f, "out_bus", SOURCE_BUS, "amp", 1.0])

      # Rendered time, not wall clock: headless drops blocks when it falls
      # behind, so on a loaded machine `duration` of wall clock is less than
      # `duration` of audio and the capture ends up cut short. The source
      # frees itself at the end of its buffer, the same block the capture
      # fills, so its /n_end is the render genuinely being finished.
      await_node_end(SOURCE_NODE, timeout: duration * 10 + 30)
      send_osc("/n_free", MIXER_NODE)
      send_osc("/n_free", CAPTURE_NODE)
      sleep 0.2

      out = File.join(Dir.tmpdir, "sonic-pi-mix-capture-#{@port}.wav")
      send_osc("/b_write", CAPTURE_BUF, out, "wav", "float", -1, 0, 0)
      await("/done")
      read_wav(out)
    end

    def read_wav(path)
      require_wavefile
      reader = WaveFile::Reader.new(path)
      buffer = reader.read(reader.total_sample_frames)
      reader.close
      samples = buffer.samples
      Capture.new(samples.map { |s| s[0] }, samples.map { |s| s[1] }, SAMPLE_RATE)
    end

    private

    def require_wavefile
      return if defined?(WaveFile)
      $LOAD_PATH.unshift File.join(ROOT, "app/server/ruby/vendor/wavefile-0.8.1/lib")
      require 'wavefile'
    end

    def send_osc(*msg)
      @server.send("127.0.0.1", @port, *msg)
    end

    # Multi-message bundle with an immediate timetag. The OSC encoder only
    # offers single-message bundles, so the envelope is assembled here.
    IMMEDIATE = [0, 1].pack("N2").freeze

    def send_bundle(*messages)
      encoder = @server.encoder
      body = messages.map do |address, *args|
        encoded = encoder.encode_single_message(address, args)
        [encoded.bytesize].pack("N") + encoded
      end.join
      packet = "#bundle\0".b + IMMEDIATE + body.b
      @raw ||= begin
        s = UDPSocket.new
        s.connect("127.0.0.1", @port)
        s
      end
      @raw.send(packet, 0)
    end

    # Both the engine's control port and the harness's reply port have to be free.
    def check_port_free
      [@port, @port + 1].each do |p|
        begin
          probe = UDPSocket.new
          probe.bind("127.0.0.1", p)
          probe.close
        rescue Errno::EADDRINUSE, SystemCallError
          probe.close rescue nil
          raise EngineError,
                "port #{p} is already in use: an engine from an earlier run is " \
                "probably still alive. Check for stray 'supersonic' processes."
        end
      end
    end

    def drain_replies
      loop { @replies.pop(true) }
    rescue ThreadError
      nil
    end

    # Node notifications are off by default, and /n_end is how a render knows
    # it is done.
    def enable_notifications
      send_osc("/notify", 1)
      await("/done")
    end

    # /n_end carries the node id as the first argument.
    def await_node_end(node_id, timeout:)
      deadline = Time.now + timeout
      loop do
        remaining = deadline - Time.now
        raise EngineError, "render never finished: no /n_end for node #{node_id}; " \
                           "see #{@log_path}" if remaining <= 0
        begin
          reply = Timeout.timeout(remaining) { @replies.pop }
        rescue Timeout::Error
          next
        end
        raise EngineError, "engine reported #{reply.inspect}" if reply.first == "/fail"
        next unless reply.first == "/n_end"
        return if reply.last.first.first == node_id
      end
    end

    def await(address, timeout: 5)
      deadline = Time.now + timeout
      loop do
        remaining = deadline - Time.now
        raise EngineError, "timed out waiting for #{address}" if remaining <= 0
        begin
          reply = Timeout.timeout(remaining) { @replies.pop }
        rescue Timeout::Error
          raise EngineError, "timed out waiting for #{address}"
        end
        return reply if reply.first == address
        raise EngineError, "engine reported #{reply.inspect}" if reply.first == "/fail"
      end
    end

    def wait_for_boot(timeout: 15)
      deadline = Time.now + timeout
      loop do
        raise EngineError, "engine did not boot; see #{@log_path}" if Time.now > deadline
        send_osc("/status")
        begin
          Timeout.timeout(0.5) do
            loop { break if @replies.pop.first == "/status.reply" }
          end
          return
        rescue Timeout::Error
          next
        end
      end
    end

    # /b_allocRead is served by the sample loader, which sends no /done, so
    # the only way to know the buffer arrived is to ask.
    def confirm_buffer(bufnum, timeout: 5)
      deadline = Time.now + timeout
      loop do
        raise EngineError, "buffer #{bufnum} never loaded" if Time.now > deadline
        send_osc("/b_query", bufnum)
        begin
          reply = Timeout.timeout(0.5) { @replies.pop }
        rescue Timeout::Error
          next
        end
        next unless reply.first == "/b_info"
        frames = reply.last.first[1]
        return if frames && frames > 0
      end
    end

    def load_synthdefs
      unless Dir.exist?(HARNESS_DEFS) && !Dir.empty?(HARNESS_DEFS)
        raise EngineError, "harness synthdefs missing: run #{File.expand_path('../synthdefs/build.sh', __dir__)}"
      end
      send_osc("/d_loadDir", HARNESS_DEFS)
      await("/done")
    end
  end
end

require 'timeout'
