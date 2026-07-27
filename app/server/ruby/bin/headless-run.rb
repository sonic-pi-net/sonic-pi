## Headless Sonic Pi runner for automated timing/behaviour tests.
##
## Usage: ruby headless-run.rb /path/to/script.rb [duration_seconds]
##
## Boots the daemon + engine, waits until ready, runs the given script,
## streams every server log/error line to stdout (prefixed + timestamped
## relative to script start) for `duration` seconds, then shuts down cleanly.
## Unlike repl.rb there is no Readline loop, so it runs unattended.

require 'open3'
require 'monitor'
require_relative "../lib/sonicpi/osc/osc"
require_relative "../paths"
require_relative "../lib/sonicpi/promise"

module SonicPi
  class HeadlessRun
    def initialize(code, duration)
      @out = Monitor.new
      @t0 = nil
      @server_started = Promise.new
      @engine_started = Promise.new

      daemon_stdin, daemon_out, daemon_wait = Open3.popen2e Paths.ruby_path, Paths.daemon_path
      say "daemon pid #{daemon_wait.pid}"

      info_prom = Promise.new
      Thread.new do
        daemon_out.each do |line|
          info_prom.deliver!(line.force_encoding("UTF-8")) rescue nil
        end
      end
      info = info_prom.get.split.map(&:to_i)
      daemon_port, gui_listen, gui_send, _sc, cues, token = info
      say "cues port #{cues}, token #{token}"

      eval_client = OSC::UDPClient.new("localhost", gui_send)
      incoming = OSC::UDPServer.new(gui_listen)
      add_handlers!(incoming)

      # keep-alive so the daemon doesn't reap us mid-test
      Thread.new do
        kc = OSC::UDPClient.new("localhost", daemon_port)
        at_exit { kc.send("/daemon/exit", token) rescue nil }
        loop { kc.send("/daemon/keep-alive", token); sleep 4 }
      end

      say "waiting for server..."
      Thread.new do
        until @server_started.delivered?
          eval_client.send("/ping", token, "hello") rescue nil
          sleep 0.5
        end
      end
      @server_started.get
      @engine_started.get
      eval_client.send("/mixer-amp", token, 0.0, 0)  # silent: master amp 0
      say "READY — running script for #{duration}s"

      @t0 = Time.now.to_f
      eval_client.send("/run-code", token, code)

      sleep duration
      say "done — stopping"
      eval_client.send("/stop-all-jobs", token) rescue nil
      sleep 0.5
      exit 0
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
        # The engine no longer pushes /supersonic/info unprompted (it now
        # replies to the GUI's /supersonic/setup), so treat the spider's
        # final boot message as engine-ready too.
        @engine_started.deliver!(true) rescue nil if m[1].to_s.include?("Live Coding begin")
      end

      osc.add_method("/flash") { |m| say "FLASH job #{m[0]} #{m[1]} line #{m[2]}" }

      osc.add_method("/live_loop/scope") { |m| say "LOOP-SCOPE job #{m[0]} #{m[1]} #{m[2]} line #{m[3]} slot #{m[4]}" }
      osc.add_method("/live_loop/scope-ended") { |m| say "LOOP-SCOPE-ENDED job #{m[0]} #{m[1]}" }

      osc.add_method("/log/multi_message") do |m|
        next if m == "" || !m.is_a?(Array)
        thread = m[1]
        msgs = m[4..-1] || []
        # msgs is [colour, text, colour, text, ...]
        texts = []
        msgs.each_slice(2) { |_c, t| texts << t if t }
        label = (thread.to_s.empty? || thread == "\"\"") ? "run" : thread
        texts.each { |t| say "#{label}: #{t}" }
      end

      osc.add_method("/error") do |m|
        say "ERROR run #{m[0]} line #{m[3]}: #{m[1]}"
        say "  #{m[2]}"
      end

      osc.add_method("/syntax_error") do |m|
        say "SYNTAX ERROR run #{m[0]} line #{m[3]}: #{m[1]} | #{m[2]}"
      end
    end
  end
end

script = ARGV[0]
duration = (ARGV[1] || "20").to_f
abort "File not found: #{script}" unless script && File.exist?(script)
SonicPi::HeadlessRun.new(File.read(script), duration)
