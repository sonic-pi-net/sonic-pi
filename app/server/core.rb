#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

raise "Sonic Pi requires Ruby 1.9.3+ to be installed. You are using version #{RUBY_VERSION}" if RUBY_VERSION < "1.9.3"

## This core file sets up the load path and applies any necessary monkeypatches.

## Ensure native lib dir is available
os = case RUBY_PLATFORM
     when /.*arm.*-linux.*/
       :raspberry
     when /.*linux.*/
       :linux
     when /.*darwin.*/
       :osx
     when /.*mingw.*/
       :windows
     else
       RUBY_PLATFORM
     end
$:.unshift "#{File.expand_path("../rb-native", __FILE__)}/#{os}/#{RUBY_VERSION}p#{RUBY_PATCHLEVEL}/"

## Ensure all libs in vendor directory are available
Dir["#{File.expand_path("../vendor", __FILE__)}/*/lib/"].each do |vendor_lib|
  $:.unshift vendor_lib
end

require 'did_you_mean' unless RUBY_VERSION < "2.0.0"

require 'osc-ruby'

module SonicPi
  module Core
    module ThreadLocalCounter
      def self.get_or_create_counters
        counters = Thread.current.thread_variable_get(:sonic_pi_core_thread_local_counters)
        unless counters
          counters = {}
          Thread.current.thread_variable_set(:sonic_pi_core_thread_local_counters, counters)
        end
        return counters
      end

      def self.tick(k, n=1)
        counters = get_or_create_counters
        if counters[k]
          return counters[k] = counters[k] + n
        else
          return counters[k] = 0
        end
      end

      def self.read(k)
        counters = get_or_create_counters
        counters[k] || 0
      end

      def self.set(k, v)
        counters = get_or_create_counters
        counters[k] = v
      end
    end
  end
end


module SonicPi
  module Core
    class RingArray < Array
      def [](idx, len=nil)
        return self.to_a[idx, len] if len

        idx = idx.to_i % size if idx.is_a? Numeric
        self.to_a[idx]
      end

      # TODO - ensure this returns a ring array
      def slice(idx, len=nil)
        return self.to_a.slice(idx, len) if len

        idx = idx.to_i % size if idx.is_a? Numeric
        self.to_a.slice(idx)
      end

      def ring
        self
      end

      def to_a
        Array.new(self)
      end

      def tick(key, n=1)
        idx = ThreadLocalCounter.tick(key, n)
        self[idx]
      end

      def hook(key, *args)
        idx = ThreadLocalCounter.read(key)
        self[idx]
      end

      def to_s
        inspect
      end

      def inspect
        a = self.to_a
        if a.empty?
          "(ring)"
        else
          "(ring #{a.inspect[1...-1]})"
        end
      end

      #TODO:    def each_with_ring
    end
  end
end


class String
  def shuffle
    self.chars.to_a.shuffle.join
  end
end

class Symbol
  def shuffle
    self.to_s.shuffle.to_sym
  end
end

class Float
  def times(&block)
    self.to_i.times do |idx|
      yield idx.to_f
    end
  end
end

module OSC
  class ServerOverTcp < Server

    def initialize(port)
      puts "port #{port}"
      @server = TCPServer.open(port)
      @matchers = []
      @queue = Queue.new
    end

    def safe_detector
      @server.listen(5)
      loop do
        @so ||= @server.accept
        begin
          read_all = false
          osc_data = ""
          while(!read_all) do
            readfds, _, _ = select([@so], nil, nil, 0.1)
            if readfds
              packet_size = @so.recv(4)
              packet_size.force_encoding("BINARY")
              size = packet_size.unpack('N')[0]

              if size && size > 0
                puts "size: #{size}"
                result = @so.recv(size)
                if result != ""
                  osc_data = result
                  read_all = true
                else
                  puts "Connection closed by client"
                  @so.close
                  @so = nil
                  break
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
        rescue Errno::ECONNREFUSED => e
          puts "Waiting for OSC server..."
          sleep(1)
        end
      end
      @so
    end

  end
end

#Monkeypatch osc-ruby to add sending skills to Servers
#https://github.com/samaaron/osc-ruby/commit/bfc31a709cbe2e196011e5e1420827bd0fc0e1a8
#and other improvements
module OSC

  class Client
    def send_raw(mesg)
      @so.send(mesg, 0)
    end
  end

  class Server
    def send(msg, address, port)
      @socket.send msg.encode, 0, address, port
    end

    def send_raw(msg, address, port)
      @socket.send msg, 0, address, port
    end

    def initialize(port, open=false)
      @socket = UDPSocket.new
      if open
        @socket.bind('', port )
      else
        @socket.bind('localhost', port )
      end
      @matchers = []
      @queue = Queue.new
    end

    def safe_detector
      loop do
        begin
          osc_data, network = @socket.recvfrom( 16384 )
          ip_info = Array.new
          ip_info << network[1]
          ip_info.concat(network[2].split('.'))
          OSCPacket.messages_from_network( osc_data, ip_info ).each do |message|
            @queue.push(message)
          end
        rescue Exception => e
          Kernel.puts e.message
        end
      end
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
  end

  class OSCDouble64 < OSCArgument
    def tag() 'd' end
    def encode() [@val].pack('G').force_encoding("BINARY") end
  end

  class OSCInt64 < OSCArgument
    def tag() 'h' end
    def encode() [@val].pack('q>').force_encoding("BINARY") end
  end

  class OSCPacket

    def self.messages_from_network( string, ip_info=nil )
      messages = []
      osc = new( string )

      if osc.bundle?
        osc.get_string #=> bundle
        time = osc.get_timestamp

        osc.get_bundle_messages.each do | message |
          begin
            msg = decode_simple_message( time, OSCPacket.new( message ) )
            if ip_info
              # Append info for the ip address
              msg.ip_port = ip_info.shift
              msg.ip_address = ip_info.join(".")
            end
            messages << msg
          rescue Exception => e
            Kernel.puts e.message
            Kernel.puts e.backtrace.inspect
          end
        end

      else
        begin
          msg = decode_simple_message( time, osc )
          if ip_info
            # Append info for the ip address
            msg.ip_port = ip_info.shift
            msg.ip_address = ip_info.join(".")
          end
          messages << msg
        rescue Exception => e
          Kernel.puts e.message
          Kernel.puts e.backtrace.inspect
        end
      end
      return messages
    end

    def initialize( string )
      @packet = NetworkPacket.new( string )

      @types = {
        "i" => lambda{OSCInt32.new(    get_int32   )},
        "f" => lambda{OSCFloat32.new(  get_float32 )},
        "s" => lambda{OSCString.new(   get_string  )},
        "b" => lambda{OSCBlob.new(     get_blob    )},
        "d" => lambda{OSCDouble64.new( get_double64)},
        "h" => lambda{OSCInt64.new(    get_int64   )}
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

    def get_int64
      f = @packet.getn(8).unpack('q>')[0]
      @packet.skip_padding
      f
    end

  end
end


require 'rubame'

## Teach Rubame::Server#run to block on IO.select
## and therefore not thrash round in a loop
module Rubame

  class Server
    def run(time = 0, &blk)
      readable, writable = IO.select(@reading, @writing)

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
  end
end


# Backport Ruby 2+ thread local variable syntax
if RUBY_VERSION < "2"
  class Thread
    def thread_variable_get(n)
      self[n]
    end

    def thread_variable_set(n, v)
      self[n] = v
    end

    def thread_variables
      self.keys
    end
  end
end

class Array

  def ring
    SonicPi::Core::RingArray.new(self)
  end

  def choose
    rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
    self[rgen.rand(self.size)]
  end

  alias_method :__orig_sample__, :sample
  def sample(*args, &blk)
    rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
    if rgen
      self[rgen.rand(self.size)]
    else
      __orig_sample__ *args, &blk
    end
  end

  alias_method :__orig_shuffle__, :shuffle
  def shuffle(*args, &blk)
    rgen = Thread.current.thread_variable_get :sonic_pi_spider_random_generator
    if rgen
      __orig_shuffle__(random: rgen)
    else
      __orig_shuffle__ *args, &blk
    end
  end
end


# Meta-glasses from our hero Why to help us
# see more clearly..
class Object
  # The hidden singleton lurks behind everyone
  def metaclass; class << self; self; end; end

  def meta_eval &blk; metaclass.instance_eval &blk; end
  # Adds methods to a metaclass
  def meta_def name, &blk
    meta_eval { define_method name, &blk }
  end
  # Defines an instance method within a class
  def class_def name, &blk
    class_eval { define_method name, &blk }
  end
end
