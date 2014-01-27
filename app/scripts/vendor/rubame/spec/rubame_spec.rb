require 'spec_helper'

describe Rubame::Server do
  it 'should create a new server' do
    server = Rubame::Server.new("0.0.0.0", 29929)
    server.class.should == Rubame::Server
    server.stop
  end

  it 'should receive a new client connecting' do
    server = Rubame::Server.new("0.0.0.0", 29929)

    client = TCPSocket.new 'localhost', 29929
    handshake = WebSocket::Handshake::Client.new(:url => 'ws://127.0.0.1:29929')
    client.write handshake.to_s
    connected = false

    server.run do |client|
      client.onopen do
        connected = true
      end
    end
    connected.should == true

    server.stop
  end

  it 'should send handshake replies to 10 clients' do
    server = Rubame::Server.new("0.0.0.0", 29929)

    clients = {}
    finished = false

    Thread.new do
      while server
        server.run
      end
    end

    sleep(0.3)

    10.times do
      client = TCPSocket.new 'localhost', 29929
      handshake = WebSocket::Handshake::Client.new(:url => 'ws://127.0.0.1:29929')
      clients[client] = handshake
      client.write handshake.to_s
      while line = client.gets
        handshake << line
        break if handshake.finished?
      end
      handshake.finished?.should == true
    end

    clients.each do |s, h|
      s.close
    end
    server.stop
    server = nil
    finished = true
  end

  it 'should be able to send a message to a client' do
    server = Rubame::Server.new("0.0.0.0", 29929)

    finished = false

    Thread.new do
      while server
        server.run do |client|
          client.onopen do
            client.send "Tester"
          end
        end
      end
    end

    sleep(0.3)

    client = TCPSocket.new 'localhost', 29929
    handshake = WebSocket::Handshake::Client.new(:url => 'ws://127.0.0.1:29929')
    client.write handshake.to_s
    while line = client.gets
      handshake << line
      break if handshake.finished?
    end
    handshake.finished?.should == true

    frame = WebSocket::Frame::Incoming::Client.new(:version => handshake)
    waiting = true
    time_started = Time.now
    time_passed = 0.0
    while waiting
      r, w = IO.select([client], [], nil, 0)
      time_passed = Time.now - time_started
      if r
        r.each do |s|
          pairs = client.recvfrom(20000)
          frame << pairs[0]
          # puts frame.next
          waiting = false
        end
      end
      waiting = false if time_passed > 4
    end

    (/Tester/ =~ frame.to_s).class.should eq(Fixnum)

    client.close
    server.stop
    server = nil
    finished = true
  end

  it 'should send lazy items after immediate ones' do
    server = Rubame::Server.new("0.0.0.0", 29929)

    finished = false

    Thread.new do
      while server
        server.run do |client|
          client.onopen do
            (0..3).each do |x|
              client.lazy_send("#{x}")
            end
            client.send("4")
          end
        end
      end
    end

    sleep(0.3)

    client = TCPSocket.new 'localhost', 29929
    handshake = WebSocket::Handshake::Client.new(:url => 'ws://127.0.0.1:29929')
    client.write handshake.to_s
    while line = client.gets
      handshake << line
      break if handshake.finished?
    end
    handshake.finished?.should == true

    frame = WebSocket::Frame::Incoming::Client.new(:version => handshake)
    waiting = true
    time_started = Time.now
    time_passed = 0.0
    while waiting
      r, w = IO.select([client], [], nil, 0)
      time_passed = Time.now - time_started
      if r
        r.each do |s|
          pairs = client.recvfrom(20000)
          frame << pairs[0]
          # puts frame.next
          waiting = false if (/.*4.*0.*1.*2.*3.*/ =~ frame.to_s)
        end
      end
      waiting = false if time_passed > 4
    end

    (/.*4.*0.*1.*2.*3.*/ =~ frame.to_s).class.should eq(Fixnum)

    client.close
    server.stop
    server = nil
    finished = true
  end
end
