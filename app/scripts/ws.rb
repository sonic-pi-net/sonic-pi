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
      when "stop-jobs"
        exec_stop(data)
      when "event"
        exec_event(data)
      when "sync"
        exec_sync(data)
      when "reload"
        exec_reload
      else
        raise "Unknown command: #{cmd}"
      end
    end
  end

  private

  def exec_sync(data)
    @spider.__sync(data[:val], data[:result])
  end

  def exec_stop(data)
    @spider.__stop
  end

  def exec_cmd(data)
    @spider.__spider_eval data[:val]
  end

  def exec_event(data)
    @event_queue.push data
  end

  def exec_reload
    dir = File.dirname("#{File.absolute_path(__FILE__)}")
    Dir["#{dir}/sonicpi/**/*.rb"].each do |d|
      load d
      puts "loaded #{d}"
    end
    puts "reloaded"
  end
end

$rd = RcvDispatch.new($sp, ws_out)
$clients = []

# Send stuff out from Sonic Pi jobs out to GUI
Thread.new do
  loop do
    begin
      message = ws_out.pop
      if debug_mode
        raise "message not a Hash!" unless message.is_a? Hash
      end
      message[:ts] = Time.now.strftime("%H:%M:%S")

      if debug_mode
        puts "sending:"
        puts "#{message.to_edn}"
        puts "---"
      end
      $clients.each{|c| c.send(message.to_edn)}
    rescue Exception => e
      puts e.message
      puts e.backtrace.inspect
    end
  end
end

# Receive events from the GUI to Sonic Pi (potentially creating new jobs)
server = Rubame::Server.new("0.0.0.0", 25252)

while true
  server.run do |client|
    client.onopen do
      client.send({:type => :message, :val => "Connection initiated..."}.to_edn)
      $clients << client
      puts "New Websocket Client: \n#{client.frame} \n #{client.socket} \n"

    end
    client.onmessage do |msg|
      puts "====> #{msg}" if debug_mode
      $rd.dispatch EDN.read(msg)
    end
    client.onclose do
      $clients.delete client
      warn("Connection closed...")
    end
  end
end
