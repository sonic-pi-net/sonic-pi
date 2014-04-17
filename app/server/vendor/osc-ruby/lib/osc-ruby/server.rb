module OSC
  class  Server
    def initialize( port )
      @socket = UDPSocket.new
      @socket.bind( '', port )
      @matchers = []
      @queue = Queue.new
    end

    def run
      start_dispatcher

      start_detector
    end

    def stop
      @socket.close
    end

    def add_method( address_pattern, &proc )
      matcher = AddressPattern.new( address_pattern )

      @matchers << [matcher, proc]
    end

private

    def start_detector
      begin
	      detector
      rescue
	      Thread.main.raise $!
      end
    end

    def start_dispatcher
      Thread.fork do
	      begin
	        dispatcher
	      rescue
	        Thread.main.raise $!
	      end
      end
    end

    def dispatcher
      loop do
	      mesg = @queue.pop
        dispatch_message( mesg )
      end
    end

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

    def detector
      loop do
	      osc_data, network = @socket.recvfrom( 16384 )
	      begin
          ip_info = Array.new
          ip_info << network[1]
          ip_info.concat(network[2].split('.'))
	        OSCPacket.messages_from_network( osc_data, ip_info ).each do |message|
	          @queue.push(message)
          end

	      rescue EOFError
	      end
      end
    end

  end
end