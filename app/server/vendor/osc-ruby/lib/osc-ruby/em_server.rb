require 'eventmachine'

module OSC
  Channel = EM::Channel.new

  class Connection < EventMachine::Connection
    def receive_data data
     ip_info = get_peername[2,6].unpack "nC4"  
     Channel << OSC::OSCPacket.messages_from_network( data, ip_info )
    end
  end

  class EMServer

    def initialize( port = 3333 )
      @port = port
      setup_dispatcher
      @tuples = []
    end

    def run
      EM::run { EM::open_datagram_socket "0.0.0.0",  @port, Connection }
    end

    def add_method(address_pattern, &proc)
      matcher = AddressPattern.new( address_pattern )

      @tuples << [matcher, proc]
    end

  private
    def setup_dispatcher
      Channel.subscribe do  |messages|
        messages.each do |message|
          diff =  ( message.time || 0 ) - Time.now.to_ntp

          if diff <=  0
            sendmesg( message )
          else
            EM.defer  do
              sleep(  diff  )
              sendmesg( message )
            end
          end
        end
      end
    end

    def sendmesg(mesg)
      @tuples.each do |matcher, obj|
        if matcher.match?( mesg.address )
          obj.call( mesg )
        end
      end
    end
  end
end






