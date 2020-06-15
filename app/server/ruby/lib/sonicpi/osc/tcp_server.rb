#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  module OSC
    class ServerOverTcp < Server

      def initialize(port)
        puts "port #{port}"
        @server = TCPServer.open(port)
        @server.setsockopt(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)

        @matchers = []
        @queue = Queue.new
      end

      def safe_detector
        @server.listen(5)
        Kernel.loop do
          @so ||= @server.accept
          begin
            read_all = false
            while(!read_all) do
              readfds, _, _ = select([@so], nil, nil, 0.1)
              if readfds
                packet_size = @so.recv(4)
                if(packet_size.length < 4)
                  if(packet_size.length == 0)
                    puts "Connection dropped"
                  else
                    puts "Failed to read full 4 bytes. Length: #{packet_size.length} Content: #{packet_size.unpack("b*")}"
                  end
                  @so.close
                  @so = nil
                  break
                else
                  packet_size.force_encoding("BINARY")
                  bytes_expected = packet_size.unpack('N')[0]
                  bytes_read = 0
                  osc_data = ""
                  while(bytes_read < bytes_expected) do
                    result = @so.recv(bytes_expected)
                    #puts "bytes expected: #{bytes_expected} bytes read: #{result.length}"
                    if result.length <= 0
                      puts "Connection closed by client"
                      @so.close
                      @so = nil
                      break
                    else
                      #puts "message: #{result}"
                      bytes_read += result.length
                      osc_data += result
                      if bytes_read == bytes_expected
                        read_all = true
                      end
                    end
                  end
                end
              end
            end

            if read_all
              OSCPacket.messages_from_network( osc_data ).each do |message|
                @queue.push(message)
              end
            end
          rescue Exception => e
            puts e
            Kernel.puts e.message
          end
        end
      end

      def stop
        @so.close if @so
        @server.close
      end

      def safe_run
        Thread.fork do
          begin
            dispatcher
          rescue Exception => e
            Kernel.puts e.message
            Kernel.puts e.backtrace.inspect
          end
        end
        safe_detector
      end

      def run
        start_dispatcher

        start_detector
      end

      def add_method( address_pattern, &proc )
        matcher = AddressPattern.new( address_pattern )

        @matchers << [matcher, proc]
      end

      private

      def dispatch_message( message )
        diff = ( message.time || 0 ) - Time.now.to_ntp

        if diff <= 0
          sendmesg( message)
        else # spawn a thread to wait until it's time
          Thread.fork do
            sleep( diff )
            sendmesg( mesg )
            Thread.exit
          end
        end
      end

      def sendmesg(mesg)
        @matchers.each do |matcher, proc|
          if matcher.match?( mesg.address )
            proc.call( mesg )
          end
        end
      end
    end
  end
end
