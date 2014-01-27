require 'rubygems'
require 'sinatra'
require 'sinatra-websocket'
require 'thread'
require 'edn'
require 'cgi'
require 'fastimage'
require 'osc-ruby'

require_relative "lib/sonicpi/scsynth"
require_relative "lib/sonicpi/studio"
require_relative "lib/sonicpi/spider"
require_relative "lib/sonicpi/server"
require_relative "lib/sonicpi/util"

Thread.abort_on_exception=true

#Monkeypatch osc-ruby to add sending skills to Servers
#https://github.com/samaaron/osc-ruby/commit/bfc31a709cbe2e196011e5e1420827bd0fc0e1a8
#and other improvements
module OSC

  class Server
    def send(msg, address, port)
      @socket.send msg.encode, 0, address, port
    end
  end

  class OSCDouble64 < OSCArgument
    def tag() 'd' end
    def encode() [@val].pack('G').force_encoding("BINARY") end
  end

  class OSCPacket

    def initialize( string )
      @packet = NetworkPacket.new( string )

      @types = { "i" => lambda{  OSCInt32.new(   get_int32 ) },
        "f" => lambda{  OSCFloat32.new( get_float32 ) },
        "s" => lambda{  OSCString.new(  get_string ) },
        "b" => lambda{  OSCBlob.new(    get_blob )},
        "d" => lambda{  OSCDouble64.new(   get_double64 )}
      }
    end

    def get_arguments
      if @packet.getc == ?,

        tags = get_string
        args = []

        tags.scan(/./) do | tag |
          type_handler = @types[tag]
          raise "Unknown OSC type: #{tag}" unless type_handler
          args << type_handler.call
        end
        args
      end
    end

    def get_double64
      f = @packet.getn(8).unpack('G')[0]
      @packet.skip_padding
      f
    end

  end
end

include SonicPi::Util

set :server, 'thin'
set :sockets, []
set :bind, '0.0.0.0'

ws_out = Queue.new
$scsynth = SonicPi::SCSynth.instance
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

  def exec_photo(data)
    name = "#{Time.now.strftime("%Y-%m-%d-%H-%M-%S")}-#{rand(100)}"
    file = "/home/pi/sinatratest/public/#{name}.jpg"
    `raspistill -t 0 -o #{file}` if os == :linux
#    @out_queue.push({kind: :image, val: "meta-ex2.JPG"})
    width, height = FastImage.size(file)
    @out_queue.push({kind: :image, val: "#{name}.jpg", width: width, height: height, scale: 0.1})
  end

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

get '/' do
  if !request.websocket?
    erb :index
  else
    request.websocket do |ws|
      ws.onopen do
        ws.send({:type => :message, :val => "Connection initiated..."}.to_edn)
        settings.sockets << ws
      end

      ws.onmessage do |msg|
        puts "====> #{msg}" if debug_mode
        $rd.dispatch EDN.read(msg)
      end
      ws.onclose do
        warn("Connection closed...")
        settings.sockets.delete(ws)
      end
    end
  end
end

get '/reset' do
  $c.clear_scsynth!
  "Scsynth reset"
end

get '/hi' do
  "Hello Worlds!"
end

post '/evalcode' do
  code = params[:foo]

  res = sp.spider_eval(code)
  "Here's your result: #{res} :-)"
end

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
      EM.next_tick { settings.sockets.each{|s| s.send(message.to_edn)}}
    rescue Exception => e
      puts e.message
      puts e.backtrace.inspect
    end
  end
end
