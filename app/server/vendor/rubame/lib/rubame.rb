require 'websocket'
require 'socket'
require 'fiber'

module Rubame
  class Server
    def initialize(host, port)
      Socket.do_not_reverse_lookup
      @hostname = host
      @port = port

      @reading = []
      @writing = []

      @clients = {} # Socket as key, and Client as value

      @socket = TCPServer.new(@hostname, @port)
      @reading.push @socket
    end

    def accept
      socket = @socket.accept_nonblock
      @reading.push socket
      handshake = WebSocket::Handshake::Server.new
      client = Rubame::Client.new(socket, handshake, self)
      
      while line = socket.gets
        client.handshake << line
        break if client.handshake.finished?
      end
      if client.handshake.valid?
        @clients[socket] = client
        client.write handshake.to_s
        client.opened = true
        return client
      else
        close(client)
      end
      return nil
    end

    def read(client)

      pairs = client.socket.recvfrom(2000)
      messages = []

      if pairs[0].length == 0
        close(client)
      else
        client.frame << pairs[0]

        while f = client.frame.next
          if (f.type == :close)
            close(client)
            return messages
          else
            messages.push f
          end
        end
        
      end

      return messages

    end

    def close(client)
      @reading.delete client.socket
      @clients.delete client.socket
      begin
        client.socket.close
      rescue
      end
      client.closed = true
    end

    def run(time = 0, &blk)
      readable, writable = IO.select(@reading, @writing, nil, 0)

      if readable
        readable.each do |socket|
          client = @clients[socket]
          if socket == @socket
            client = accept
          else
            msg = read(client)
            client.messaged = msg
          end

          blk.call(client) if client and blk
        end
      end

      # Check for lazy send items
      timer_start = Time.now
      time_passed = 0
      begin
        @clients.each do |s, c|
          c.send_some_lazy(5)
        end
        time_passed = Time.now - timer_start
      end while time_passed < time
    end

    def stop
      @socket.close
    end
  end

  class Client
    attr_accessor :socket, :handshake, :frame, :opened, :messaged, :closed

    def initialize(socket, handshake, server)
      @socket = socket
      @handshake = handshake
      @frame = WebSocket::Frame::Incoming::Server.new(:version => @handshake.version)
      @opened = false
      @messaged = []
      @lazy_queue = []
      @lazy_current_queue = nil
      @closed = false
      @server = server
    end

    def write(data)
      @socket.write data
    end

    def send(data)
      frame = WebSocket::Frame::Outgoing::Server.new(:version => @handshake.version, :data => data, :type => :text)
      begin
        @socket.write frame
        @socket.flush
      rescue
        @server.close(self) unless @closed
      end
    end

    def lazy_send(data)
      @lazy_queue.push data
    end

    def get_lazy_fiber
      # Create the fiber if needed
      if @lazy_fiber == nil or !@lazy_fiber.alive?
        @lazy_fiber = Fiber.new do
          @lazy_current_queue.each do |data|
            send(data)
            Fiber.yield unless @lazy_current_queue[-1] == data
          end
        end
      end

      return @lazy_fiber
    end

    def send_some_lazy(count)
      # To save on cpu cycles, we don't want to be chopping and changing arrays, which could get quite large.  Instead,
      # we iterate over an array which we are sure won't change out from underneath us.
      unless @lazy_current_queue
        @lazy_current_queue = @lazy_queue
        @lazy_queue = []
      end

      completed = 0
      begin
        get_lazy_fiber.resume
        completed += 1
      end while (@lazy_queue.count > 0 or @lazy_current_queue.count > 0) and completed < count

    end

    def onopen(&blk)
      if @opened
        begin
          blk.call
        ensure
          @opened = false
        end
      end
    end

    def onmessage(&blk)
      if @messaged.size > 0
        begin
          @messaged.each do |x|
            blk.call(x.to_s)
          end
        ensure
          @messaged = []
        end
      end
    end

    def onclose(&blk)
      if @closed
        begin
          blk.call
        ensure
        end
      end
    end
  end

  line = 0
end

if __FILE__==$0
  server = Rubame::Server.new("0.0.0.0", 25222)
  while (!$quit)
    server.run do |client|
      client.onopen do
        puts "Server reports:  client open"
      end
      client.onmessage do |mess|
        puts "Server reports:  message received: #{mess}"
      end
      client.onclose do
        puts "Server reports:  client closed"
      end
    end
  end
end
