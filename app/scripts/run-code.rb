#!/usr/bin/env ruby

require 'rubygems'
require 'thread'
require 'osc-ruby'

require_relative "sonicpi/scsynth"
require_relative "sonicpi/studio"
require_relative "sonicpi/spider"
require_relative "sonicpi/server"
require_relative "sonicpi/util"

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

ws_out = Queue.new
# $scsynth = SonicPi::SCSynth.instance
$c = OSC::Client.new("localhost", 4556)
# $c.send(OSC::Message.new("/d_loadDir", synthdef_path))
# sleep 2

$sp =  SonicPi::Spider.new "localhost", 4556, ws_out, 5

Thread.new do
  loop do
    message = ws_out.pop
    if message[:type] == :message
      puts message[:val]
    else
      raise message.to_s
    end
  end
end


code = ARGF.read + "\nnil"
$sp.spider_eval code
