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
    class ClientOverTcp
      def initialize(host, port)
        @host, @port = host, port
      end

      def send_raw(mesg)
        so.send(mesg, 0)
      end

      def send(mesg)
        so.send(mesg.encode, 0)
      end

      def stop
        so.close
      end

      def so
        while(!@so) do
          begin
            @so = TCPSocket.new(@host, @port)
            @so.setsockopt(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)
          rescue Errno::ECONNREFUSED => e
            puts "Waiting for OSC server..."
            sleep(1)
          end
        end
        @so
      end

    end
  end
end
