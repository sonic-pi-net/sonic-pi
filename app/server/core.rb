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

begin
  require 'did_you_mean'
rescue LoadError
  warn "Could not load did_you_mean"
end

require 'osc-ruby'
require 'hamster/vector'
require 'wavefile'

module SonicPi
  module Core
    module SPRand
      # Read in same random numbers as server for random stream sync
      @@random_numbers = ::WaveFile::Reader.new(File.expand_path("../../../etc/buffers/rand-stream.wav", __FILE__), ::WaveFile::Format.new(:mono, :float, 44100)).read(441000).samples.freeze

      def self.to_a
        @@random_numbers
      end

      def self.inc_idx(init=0)
        ridx = Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_idx) || init
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, ridx + 1
        ridx
      end

      def self.dec_idx(init=0)
        ridx = Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_idx) || init
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, ridx - 1
        ridx
      end

      def self.set_seed!(seed)
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, seed
      end


      def self.get_seed
        Thread.current.thread_variable_get :sonic_pi_spider_random_gen_idx
      end

      def self.rand(max, idx=nil)
        idx = inc_idx unless idx
        # we know that the fixed rand stream has length 44100
        # also, scsynth server seems to swallow first rand
        # so always add 1 to index
        idx = (idx + 1) % 44100
        @@random_numbers[idx] * max
      end

      def self.rand_i(max, idx=nil)
        rand(max, idx).to_i
      end

    end

    module TLMixin
      def tick(*args)
        idx = SonicPi::Core::ThreadLocalCounter.tick(*args)
        self[idx]
      end

      def look(*args)
        idx = SonicPi::Core::ThreadLocalCounter.look(*args)
        self[idx]
      end
    end

    module ThreadLocalCounter
      def self.get_or_create_counters
        counters = Thread.current.thread_variable_get(:sonic_pi_core_thread_local_counters)
        return counters if counters
        counters = {}
        Thread.current.thread_variable_set(:sonic_pi_core_thread_local_counters, counters)
        counters
      end

      def self.tick(k = :___sonic_pi_default_tick_key___, *args)
        k = (k && k.is_a?(String)) ? k.to_sym : k
        if k.is_a? Symbol
          opts = args.first || {}
        else
          opts = k
          k = :___sonic_pi_default_tick_key___
        end

        raise "Tick key must be a symbol, got #{k.class}: #{k.inspect}" unless k.is_a? Symbol
        raise "Tick opts must be key value pairs, got: #{opts.inspect}" unless opts.is_a? Hash
        step = opts[:step] || 1
        offset = opts[:offset] || 0
        counters = get_or_create_counters
        if counters[k]
          curr_val, next_val = *counters[k]
          counters[k] = [next_val + step-1, next_val + step]
          return next_val + step-1 + offset
        else
          counters[k] = [step-1, step]
          return step-1 + offset
        end
      end

      def self.look(k = :___sonic_pi_default_tick_key___, *args)
        k = (k && k.is_a?(String)) ? k.to_sym : k
        if k.is_a? Symbol
          opts = args.first || {}
        else
          opts = k
          k = :___sonic_pi_default_tick_key___
        end

        raise "Tick key must be a symbol, got #{k.class}: #{k.inspect}" unless k.is_a? Symbol
        offset = opts[:offset] || 0
        counters = get_or_create_counters
        val, _ = *counters[k]
        return (val || 0) + offset
      end

      def self.set(k=:___sonic_pi_default_tick_key___, v)
        if k.is_a? Numeric
          v = k
          k = :___sonic_pi_default_tick_key___
        end
        raise "Tick key must be a symbol, got #{k.class}: #{k.inspect}" unless k.is_a? Symbol
        raise "Tick value must be a number, got #{v.class}: #{v.inspect}" unless v.is_a? Numeric
        counters = get_or_create_counters
        counters[k] = [v, v]
        v
      end

      def self.rm(k=:___sonic_pi_default_tick_key___)
        raise "Tick key must be a symbol, got #{k.class}: #{k.inspect}" unless k.is_a? Symbol
        counters = get_or_create_counters
        counters.delete(k)
        nil
      end

      def self.reset_all
        Thread.current.thread_variable_set(:sonic_pi_core_thread_local_counters, {})
        nil
      end
    end
  end
end


module SonicPi
  module Core
    class EmptyVectorError < StandardError ; end
    class InvalidIndexError < StandardError ; end

    class SPVector < Hamster::Vector
      include TLMixin
      def initialize(list)
        raise EmptyVectorError, "Cannot create an empty vector" if list.empty?
        super
      end

      def ___sp_vector_name
        "vector"
      end

      def ___sp_preserve_vec_kind(a)
        self.class.new(a)
      end

      def [](idx, len=(missing_length = true))
        raise InvalidIndexError, "Invalid index: #{idx.inspect}, was expecting a number or range" unless idx && (idx.is_a?(Numeric) || idx.is_a?(Range))
        if idx.is_a?(Numeric) && missing_length
          idx = map_index(idx)
          super idx
        else
          if missing_length
            super(idx)
          else
            super(idx, len)
          end
        end
      end

      def choose
        self[SonicPi::Core::SPRand.rand_i(self.size)]
      end

      def ring
        SonicPi::Core::RingVector.new(self)
      end

      def ramp
        SonicPi::Core::RampVector.new(self)
      end

      def to_s
        inspect
      end

      def inspect
        a = self.to_a
        if a.empty?
          "(#{___sp_vector_name})"
        else
          "(#{___sp_vector_name} #{a.inspect[1...-1]})"
        end
      end

      def to_a
        Array.new(self)
      end

      def stretch(num_its)
        res = []
        self.each do |v|
          num_its.times do
            res << v
          end
        end
        ___sp_preserve_vec_kind(res)
      end
    end

    class RingVector < SPVector
      def map_index(idx)
        idx = idx % size
        return idx
      end

      def ___sp_vector_name
        "ring"
      end
    end

    class RampVector < SPVector
      def map_index(idx)
        idx = idx.to_i
        idx = [idx, size - 1].min
        idx = [idx, 0].max
        return idx
      end

      def ___sp_vector_name
        "ramp"
      end
    end
  end
end


class String
  def shuffle
    self.chars.to_a.shuffle.join
  end

  def ring
    self.chars.ring
  end
end

class Numeric
  def max(other)
    return self if self <= other
    other
  end

  def min(other)
    return self if self >= other
    other
  end
end

class Symbol
  def shuffle
    self.to_s.shuffle.to_sym
  end

  def ring
    self.to_s.ring
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
  include SonicPi::Core::TLMixin

  def ring
    SonicPi::Core::RingVector.new(self)
  end

  def ramp
    SonicPi::Core::RampVector.new(self)
  end

  def choose
    self[SonicPi::Core::SPRand.rand_i(self.size)]
  end

  alias_method :__orig_sample__, :sample
  def sample(*args, &blk)

    if Thread.current.thread_variable_get(:sonic_pi_spider_thread)
      self[SonicPi::Core::SPRand.rand(self.size)]
    else
      __orig_sample__ *args, &blk
    end
  end

  alias_method :__orig_shuffle__, :shuffle
  def shuffle(*args, &blk)
    if Thread.current.thread_variable_get(:sonic_pi_spider_thread)
      new_a = self.dup
      s = new_a.size
      s.times do
        idx_a = SonicPi::Core::SPRand.rand(s)
        idx_b = SonicPi::Core::SPRand.rand(s)
        new_a[idx_a], new_a[idx_b] = new_a[idx_b], new_a[idx_a]
      end
      return new_a
    else
      __orig_shuffle__ *args, &blk
    end
  end

  alias_method :__orig_shuffle_bang__, :shuffle!
  def shuffle!(*args, &blk)
    if Thread.current.thread_variable_get(:sonic_pi_spider_thread)
      new_a = self.shuffle
      self.replace(new_a)
    else
      __orig_shuffle_bang__ *args, &blk
    end
  end
end



# Meta-glasses from our hero Why to help us
# see more clearly..
class Object

  def ring
    self.to_a.ring
  end

  def tick(*args)
    self.to_a.tick(*args)
  end

  def look(*args)
    self.to_a.look(*args)
  end

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
