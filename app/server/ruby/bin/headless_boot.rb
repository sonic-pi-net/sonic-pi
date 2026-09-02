## Shared boot for the headless harnesses: headless-run.rb, headless-record.rb.
##
## Spawns the daemon, waits for the spider and the engine to come up, and hands
## back the OSC clients plus a timestamped logger. Log, error and puts lines are
## streamed to stdout; a harness adds its own handlers via the block form of
## boot!.

require 'open3'
require 'monitor'
require_relative "../lib/sonicpi/osc/osc"
require_relative "../paths"
require_relative "../lib/sonicpi/promise"

module SonicPi
  class HeadlessBoot
    attr_reader :token, :eval_client, :engine_client, :errors

    def initialize
      @out = Monitor.new
      @t0 = nil
      @server_started = Promise.new
      @engine_started = Promise.new
      @errors = []
      @log_listeners = []
    end

    # Register a listener for messages emitted by Spider. This lets headless
    # front-ends consume logs without replacing the OSC handlers that manage
    # the boot handshake and console output.
    def add_log_listener(&listener)
      @log_listeners << listener if listener
    end

    def boot!
      _daemon_stdin, daemon_out, daemon_wait = Open3.popen2e Paths.ruby_path, Paths.daemon_path
      say "daemon pid #{daemon_wait.pid}"

      info_prom = Promise.new
      Thread.new do
        daemon_out.each do |line|
          info_prom.deliver!(line.force_encoding("UTF-8")) rescue nil
        end
      end
      info = info_prom.get.split.map(&:to_i)
      daemon_port, gui_listen, gui_send, sc_port, cues, @token = info
      say "cues port #{cues}, engine osc port #{sc_port}, token #{@token}"

      @eval_client   = OSC::UDPClient.new("localhost", gui_send)
      @engine_client = OSC::UDPClient.new("localhost", sc_port)

      incoming = OSC::UDPServer.new(gui_listen)
      add_handlers!(incoming)
      yield incoming if block_given?

      # keep-alive so the daemon doesn't reap us mid-run
      Thread.new do
        kc = OSC::UDPClient.new("localhost", daemon_port)
        at_exit { kc.send("/daemon/exit", @token) rescue nil }
        loop { kc.send("/daemon/keep-alive", @token); sleep 4 }
      end

      say "waiting for server..."
      Thread.new do
        until @server_started.delivered?
          @eval_client.send("/ping", @token, "hello") rescue nil
          sleep 0.5
        end
      end
      @server_started.get
      @engine_started.get
      self
    end

    def mute!
      @eval_client.send("/mixer-output-volume", @token, 0.0, 0)
    end

    def run(code)
      @t0 = Time.now.to_f
      @eval_client.send("/run-code", @token, code)
    end

    def stop_all
      @eval_client.send("/stop-all-jobs", @token) rescue nil
    end

    def stamp
      @t0 ? format("%7.2f", Time.now.to_f - @t0) : "  boot "
    end

    def say(msg)
      @out.synchronize { puts "[#{stamp}] #{msg}"; STDOUT.flush }
    end

    def add_handlers!(osc)
      osc.add_method("/supersonic/info") { |_m| @engine_started.deliver!(true) rescue nil }
      osc.add_method("/ack")             { @server_started.deliver!(true) rescue nil }

      osc.add_method("/log/info") do |m|
        say "LOG  #{m[1]}"
        notify_log_listeners(:info, m)
        # The engine no longer pushes /supersonic/info unprompted (it now
        # replies to the GUI's /supersonic/setup), so treat the spider's
        # final boot message as engine-ready too.
        @engine_started.deliver!(true) rescue nil if m[1].to_s.include?("Live Coding begin")
      end

      osc.add_method("/log/multi_message") do |m|
        next if m == "" || !m.is_a?(Array)
        thread = m[1]
        msgs = m[4..-1] || []
        # msgs is [colour, text, colour, text, ...]
        texts = []
        msgs.each_slice(2) { |_c, t| texts << t if t }
        label = (thread.to_s.empty? || thread == "\"\"") ? "run" : thread
        texts.each do |t|
          say "#{label}: #{t}"
          notify_log_listeners(:log, [label, t])
        end
      end

      osc.add_method("/error") do |m|
        @errors << "run #{m[0]} line #{m[3]}: #{m[1]}"
        say "ERROR run #{m[0]} line #{m[3]}: #{m[1]}"
        say "  #{m[2]}"
        notify_log_listeners(:error, m)
      end

      osc.add_method("/syntax_error") do |m|
        @errors << "syntax run #{m[0]} line #{m[3]}: #{m[1]}"
        say "SYNTAX ERROR run #{m[0]} line #{m[3]}: #{m[1]} | #{m[2]}"
        notify_log_listeners(:syntax_error, m)
      end
    end

    def notify_log_listeners(type, message)
      @log_listeners.each { |listener| listener.call(type, message) }
    rescue StandardError => e
      warn "HeadlessBoot log listener failed: #{e.message}"
    end
  end
end
