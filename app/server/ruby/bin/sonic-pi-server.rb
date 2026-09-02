#!/usr/bin/env ruby
#--
# Sonic Pi — Simple HTTP Server Mode (no auth)
#
# Boots the Sonic Pi daemon + Spider + SuperSonic without the Qt GUI
# and exposes a tiny HTTP API so you can code it remotely:
#
#   POST /run   { "code": "play 70; sleep 0.5; play 72" }
#   POST /stop  { }  or  { "job_id": 123 }
#   GET  /health, GET /, GET /logs
#
# Usage:
#   ruby app/server/ruby/bin/sonic-pi-server.rb [--host 127.0.0.1] [--port 8000]
#   ./bin/sonic-pi-server.sh --port 8000 --host 0.0.0.0
#
# No authentication — bind to 127.0.0.1 by default. Use --host 0.0.0.0
# only on a trusted network.
#
# Dependencies: json from Ruby's standard library, webrick, and vendored
# Sonic Pi libs. Some system Ruby installations package webrick separately.
#++

require 'webrick'
require 'json'
require 'thread'
require 'uri'

require_relative "../paths"
require_relative "headless_boot"

module SonicPi
  class ServerMode
    VERSION = "5.0.0-server"

    def initialize(host: "127.0.0.1", port: 8000)
      @host = host
      @port = port
      @boot = HeadlessBoot.new
      @logs = [] # {ts, type, msg}
      @logs_mu = Mutex.new
      @max_logs = 500
    end

    def push_log(type, msg)
      entry = {ts: Time.now.strftime("%H:%M:%S.%3N"), type: type, msg: msg}
      @logs_mu.synchronize do
        @logs << entry
        @logs.shift while @logs.size > @max_logs
      end
      # also print to stdout for journald/docker logs
      puts "[#{entry[:ts]}] [#{type}] #{msg}"
      $stdout.flush
    end

    def boot!
      puts "Sonic Pi Server Mode — booting daemon + engine..."
      puts "Host: #{@host}  Port: #{@port}"
      @boot.add_log_listener do |type, message|
        case type
        when :info
          push_log("info", message[1].to_s)
        when :log
          push_log("log", "#{message[0]}: #{message[1]}")
        when :error, :syntax_error
          push_log(type.to_s, "run #{message[0]} line #{message[3]}: #{message[1]} | #{message[2]}")
        end
      end
      @boot.boot!
      puts "Sonic Pi Server Mode — boot done (token #{@boot.token})"
      push_log("info", "Server ready — token #{@boot.token}")
    end

    def start_http!
      server = WEBrick::HTTPServer.new(
        Port: @port,
        BindAddress: @host,
        AccessLog: [],
        Logger: WEBrick::Log.new($stderr, WEBrick::Log::INFO)
      )

      # Graceful shutdown
      %w[INT TERM].each { |sig| trap(sig) { server.shutdown } }

      cors = lambda do |res|
        res['Access-Control-Allow-Origin'] = '*'
        res['Access-Control-Allow-Methods'] = 'GET, POST, OPTIONS'
        res['Access-Control-Allow-Headers'] = 'Content-Type'
      end

      json_res = lambda do |res, obj, status: 200|
        cors.call(res)
        res.status = status
        res['Content-Type'] = 'application/json'
        res.body = JSON.generate(obj)
      end

      help_payload = lambda do
        {
          name: "Sonic Pi Server",
          version: VERSION,
          status: "ok",
          booted: true,
          token: @boot.token,
          endpoints: {
            "GET /" => "this help",
            "GET /health" => "health check",
            "GET /logs?limit=100" => "recent spider logs",
            "POST /run" => '{ "code": "play 70" }',
            "POST /stop" => '{ } or { "job_id": 123 } — stop all or one job'
          },
          example: 'curl -X POST http://localhost:8000/run -H "Content-Type: application/json" -d \'{"code":"live_loop :a do; play scale(:c4,:major).tick; sleep 0.25; end"}\'',
          host: @host,
          port: @port
        }
      end

      server.mount_proc '/' do |req, res|
        # CORS preflight for any path
        if req.request_method == 'OPTIONS'
          cors.call(res)
          res.status = 204
          res.body = ''
          next
        end

        path = req.path
        method = req.request_method
        query = URI.decode_www_form(req.query_string.to_s).to_h

        if method == 'GET' && ['/', '/health', '/status'].include?(path)
          json_res.call(res, help_payload.call)
          next
        end

        if method == 'GET' && path == '/logs'
          limit = (query['limit'] || '100').to_i.clamp(1, @max_logs)
          logs = @logs_mu.synchronize { @logs.last(limit).dup }
          json_res.call(res, {logs: logs, count: @logs_mu.synchronize { @logs.size }})
          next
        end

        if method == 'GET' && path == '/version'
          json_res.call(res, {version: VERSION, booted: true})
          next
        end

        if ['/run', '/run-code', '/eval'].include?(path)
          unless method == 'POST'
            json_res.call(res, {status: "error", error: "method not allowed — use POST"}, status: 405)
            next
          end
          body = req.body || ""
          ctype = req['Content-Type'].to_s
          data = {}
          code = nil
          workspace = nil
          if ctype.include?('text/plain')
            code = body
          else
            begin
              data = body.strip.empty? ? {} : JSON.parse(body)
            rescue JSON::ParserError => e
              json_res.call(res, {status: "error", error: "invalid JSON: #{e.message}"}, status: 400)
              next
            end
            code = data["code"] || data["body"] || data["src"]
            workspace = data["workspace"] || "api"
          end
          code = query['code'] if query['code'] && (code.nil? || code.empty?)
          workspace ||= "api"
          unless code.is_a?(String) && !code.strip.empty?
            json_res.call(res, {status: "error", error: 'missing "code" (JSON string). Example: {"code":"play 70"} or text/plain body'}, status: 400)
            next
          end
          begin
            # Single OSC send with workspace — HeadlessBoot#run sends without
            # workspace, so bypass it and send explicitly.
            @boot.eval_client.send("/run-code", @boot.token, code, workspace.to_s)
            push_log("run", "POST #{path} workspace=#{workspace} code=#{code.lines.first&.strip&.slice(0,120)}")
            json_res.call(res, {status: "ok", message: "code sent", workspace: workspace})
          rescue => e
            json_res.call(res, {status: "error", error: e.message}, status: 500)
          end
          next
        end

        if ['/stop', '/stop-all', '/stop-all-jobs'].include?(path)
          unless method == 'POST'
            json_res.call(res, {status: "error", error: "method not allowed — use POST"}, status: 405)
            next
          end
          begin
            body = req.body || ""
            data = body.strip.empty? ? {} : (JSON.parse(body) rescue {})
            job_id = data["job_id"] || data["jobId"] || data["id"] || query['job_id']
            if job_id
              unless job_id.to_s.match?(/\A[1-9]\d*\z/)
                json_res.call(res, {status: "error", error: '"job_id" must be a positive integer'}, status: 400)
                next
              end
              @boot.eval_client.send("/stop-job", @boot.token, job_id.to_i)
              push_log("stop", "POST /stop job_id=#{job_id}")
              json_res.call(res, {status: "ok", message: "stopped job #{job_id}"})
            else
              @boot.stop_all
              push_log("stop", "POST /stop — stop-all-jobs")
              json_res.call(res, {status: "ok", message: "stopped all jobs"})
            end
          rescue => e
            json_res.call(res, {status: "error", error: e.message}, status: 500)
          end
          next
        end

        json_res.call(res, {status: "error", error: "not found: #{path}", hint: "GET / for help"}, status: 404)
      end

      puts ""
      puts "=============================================="
      puts " Sonic Pi Server listening on http://#{@host}:#{@port}"
      puts "   GET  /        — help"
      puts "   POST /run     — {\"code\":\"play 70\"}"
      puts "   POST /stop    — stop all (or {\"job_id\":123})"
      puts "   GET  /logs    — recent logs"
      puts " No auth — bound to #{@host} (use --host 0.0.0.0 to expose on LAN)"
      puts "=============================================="
      puts ""
      puts "Examples:"
      puts "  curl http://#{@host}:#{@port}/"
      puts "  curl -X POST http://#{@host}:#{@port}/run -H 'Content-Type: application/json' -d '{\"code\":\"play 70\"}'"
      puts "  curl -X POST http://#{@host}:#{@port}/run -H 'Content-Type: application/json' -d '{\"code\":\"live_loop :d do\\n sample :bd_haus\\n sleep 0.5\\nend\"}'"
      puts "  curl -X POST http://#{@host}:#{@port}/stop"
      puts ""

      server.start
    end

    def run
      boot!
      start_http!
    end
  end
end

if __FILE__ == $0
  host = ENV["SONIC_PI_SERVER_HOST"] || "127.0.0.1"
  port = ENV["SONIC_PI_SERVER_PORT"] || "8000"

  args = ARGV.dup
  until args.empty?
    case (a = args.shift)
    when "-h", "--help"
      puts <<~HELP
        Sonic Pi Server Mode (no auth)

        Usage: ruby #{__FILE__} [options]

        Options:
          --host HOST   Bind address (default: #{host}, env SONIC_PI_SERVER_HOST)
                        Use 0.0.0.0 to expose on your LAN (no auth!)
          --port PORT   Port (default: #{port}, env SONIC_PI_SERVER_PORT)
          -h, --help    This help

        Endpoints (all JSON, CORS enabled):
          GET  /, /health        Health + usage
          GET  /logs?limit=100   Recent spider logs / errors
          POST /run              Body: {"code":"play 70"} — run Sonic Pi code
          POST /stop             Body: {} or {"job_id":123} — stop jobs

        Examples:
          curl http://localhost:8000/
          curl -X POST http://localhost:8000/run -H 'Content-Type: application/json' \\
            -d '{"code":"live_loop :a do; play scale(:c4,:major).tick; sleep 0.25; end"}'
          curl -X POST http://localhost:8000/stop

        From source:
          ./bin/sonic-pi-server.sh --port 8000
          ./bin/sonic-pi-server.sh --host 0.0.0.0 --port 8000

      HELP
      exit 0
    when "--host"
      host = args.shift or abort "--host needs a value"
    when "--port", "-p"
      port = args.shift or abort "--port needs a value"
    when /\A--port=(.+)\z/
      port = $1
    when /\A--host=(.+)\z/
      host = $1
    else
      abort "Unknown argument: #{a} (try --help)"
    end
  end

  abort "--port must be an integer between 1 and 65535" unless port.to_s.match?(/\A\d+\z/) && (1..65_535).cover?(port.to_i)

  SonicPi::ServerMode.new(host: host, port: port.to_i).run
end
