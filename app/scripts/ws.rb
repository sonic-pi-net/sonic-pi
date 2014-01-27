#!/usr/bin/env ruby
require_relative "core.rb"

require 'rubame'
require 'edn'

require_relative "sonicpi/scsynth"
require_relative "sonicpi/studio"
require_relative "sonicpi/spider"
require_relative "sonicpi/server"
require_relative "sonicpi/util"

include SonicPi::Util

ws_out = Queue.new
$scsynth = SonicPi::SCSynth.instance

at_exit do
  puts "Exiting - shutting down scsynth server..."
  $scsynth.shutdown
end

$c = OSC::Client.new("localhost", 4556)
$c.send(OSC::Message.new("/d_loadDir", synthdef_path))
sleep 2

$sp =  SonicPi::Spider.new "localhost", 4556, ws_out, 5

class RcvDispatch
  def initialize(spider, out_queue)
    @threads = []
    @t_sem = Mutex.new
    @spider = spider
    @out_queue = out_queue
    @event_queue = @spider.event_queue
  end

  def dispatch(data)
    @t_sem.synchronize do
      cmd = data[:cmd]

      case cmd
      when "run-code"
        exec_cmd(data)
      when "stop"
        exec_stop(data)
      when "photo"
        exec_photo(data)
      when "event"
        exec_event(data)
      when "sync"
        exec_sync(data)
      else
        raise "Unknown command: #{cmd}"
      end
    end
  end

  private

  def exec_sync(data)
    @spider.sync(data[:val], data[:result])
  end

  def exec_stop(data)
    @threads.each {|t| t.kill}
    @threads = []
    @spider.stop
  end

  def exec_cmd(data)
    @threads << Thread.new do
      begin
        @spider.spider_eval data[:val]
      rescue Exception => e
        @out_queue.push({type: :error, val: e.message, backtrace: e.backtrace })
      end
    end
  end

  def exec_event(data)
    @event_queue.push data
  end
end

$rd = RcvDispatch.new($sp, ws_out)

server = Rubame::Server.new("0.0.0.0", 25252)
while true
  server.run do |client|
    client.onopen do
      client.send({:type => :message, :val => "Connection initiated..."}.to_edn)
      puts "Server reports:  client open"
    end
    client.onmessage do |msg|
      puts "====> #{msg}" if debug_mode
      $rd.dispatch EDN.read(msg)
    end
    client.onclose do
        warn("Connection closed...")
    end
  end
end
