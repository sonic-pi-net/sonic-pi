require 'open3'

require_relative "../lib/sonicpi/osc/osc"
require_relative "../paths"
require_relative "../lib/sonicpi/promise"

## This is a very simple barebones REPL for Sonic Pi
## All stdin is sent to Sonic Pi for evaluation and output is sent to stdout
## It also ensures the server is kept alive.

module SonicPi
  class Repl
    def initialize()
      daemon_stdin, daemon_stdout_and_err, daemon_wait_thr = Open3.popen2e Paths.ruby_path, Paths.daemon_path

      puts "Sonic Pi Daemon started with PID: #{daemon_wait_thr.pid}"
      puts "Log files are located at: #{Paths.log_path}"

      daemon_info_prom = Promise.new

      daemon_io_thr = Thread.new do
        daemon_stdout_and_err.each do |line|
          line = line.force_encoding("UTF-8")
          daemon_info_prom.deliver! line
          Thread.current.kill
        end
      end

      daemon_info = daemon_info_prom.get.split.map(&:to_i)

      daemon_port = daemon_info[0]
      gui_listen_to_spider_port = daemon_info[1]
      gui_send_to_spider_port = daemon_info[2]
      scsynth_port = daemon_info[3]
      osc_cues_port = daemon_info[4]
      tau_port = daemon_info[5]
      tau_booter_port = daemon_info[6]
      daemon_token = daemon_info[7]

      puts "OSC Cues port: #{osc_cues_port}"

      daemon_zombie_feeder = Thread.new do
        osc_client = OSC::UDPClient.new("localhost", daemon_port)

        at_exit do
          puts "Killing the Daemon..."
          osc_client.send("/daemon/exit", daemon_token)
        end

        loop do
          osc_client.send("/daemon/keep-alive", daemon_token)
          sleep 5
        end
      end

      repl_print_osc_server = OSC::UDPServer.new(gui_listen_to_spider_port)
      repl_print_osc_server.add_global_method do |osc_path, msg|
        puts "#{osc_path} #{msg[1]}"
      end

      repl_eval_osc_client = OSC::UDPClient.new("localhost", gui_send_to_spider_port)

      loop do
        input = gets
        repl_eval_osc_client.send("/run-code", daemon_token, input)
      end
    end
  end
end


SonicPi::Repl.new()