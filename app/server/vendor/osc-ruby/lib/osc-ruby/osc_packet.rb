require File.join( File.dirname( __FILE__ ), 'network_packet')
require 'ostruct'

module OSC
  class OSCPacket

    def self.messages_from_network( string, ip_info=nil )
      messages = []
      osc = new( string )

      if osc.bundle?
        osc.get_string #=> bundle
        time = osc.get_timestamp

        osc.get_bundle_messages.each do | message |
          msg = decode_simple_message( time, OSCPacket.new( message ) )
          if ip_info
            # Append info for the ip address
            msg.ip_port = ip_info.shift
            msg.ip_address = ip_info.join(".")
          end
          messages << msg
        end

      else
        msg = decode_simple_message( time, osc )
        if ip_info
          # Append info for the ip address
            msg.ip_port = ip_info.shift
            msg.ip_address = ip_info.join(".")
        end
        messages << msg
      end

      return messages
    end

    def self.decode_simple_message( time, osc_packet )
      address = osc_packet.get_string
      args = osc_packet.get_arguments

      Message.new_with_time(address, time, nil, *args )
    end

    def initialize( string )
      @packet = NetworkPacket.new( string )

      @types = { "i" => lambda{  OSCInt32.new(   get_int32 ) },
                 "f" => lambda{  OSCFloat32.new( get_float32 ) },
                 "s" => lambda{  OSCString.new(  get_string ) },
                 "b" => lambda{  OSCBlob.new(    get_blob )}
                }
    end

    def get_bundle_messages
      bundle_messages = []

      until @packet.eof?
        l = @packet.getn(4).unpack('N')[0]
        bundle_messages << @packet.getn(l)
      end
      bundle_messages
    end

    def get_string
      result = ''
      until (c = @packet.getc) == string_delemeter
	      result << c
      end
      @packet.skip_padding
      result
    end

    def get_timestamp
      #TODO: refactor this so a mortal can figure it out
      t1 = @packet.getn(4).unpack('N')[0]
      t2 = @packet.getn(4).unpack('N')[0]
      @packet.skip_padding

      if t1 == 0 && t2 == 1
        time = nil
      else
        time = t1 + t2.to_f / (2**32)
      end

      time
    end

    def get_arguments
      if @packet.getc == ?,

        tags = get_string
        args = []

        tags.scan(/./) do | tag |
          args << @types[tag].call
        end
        args
      end
    end

    def get_int32
      i = @packet.getn(4).unpack('N')[0]
      i -= 2**32 if i > (2**31-1)
      @packet.skip_padding
      i
    end

    def get_float32
      f = @packet.getn(4).unpack('g')[0]
      @packet.skip_padding
      f
    end

    def get_blob
      l = @packet.getn(4).unpack('N')[0]
      b = @packet.getn(l)
      @packet.skip_padding
      b
    end

    def bundle?
      !(@packet.to_s =~ /\A\#bundle/).nil?
    end

    def string_delemeter
     "\x00"
    end
  end
end