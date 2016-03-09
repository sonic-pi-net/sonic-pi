#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

raise "Sonic Pi requires Ruby 1.9.3+ to be installed. You are using version #{RUBY_VERSION}" if RUBY_VERSION < "1.9.3"

## This core file sets up the load path and applies any necessary monkeypatches.

## Ensure native lib dir is available
require 'rbconfig'
ruby_api = RbConfig::CONFIG['ruby_version']
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
ruby_gem_native_path = "#{File.expand_path("../rb-native", __FILE__)}"
ruby_gem_api_path = "#{ruby_gem_native_path}/#{os}/#{ruby_api}"

unless File.directory?(ruby_gem_api_path)
  STDERR.puts "*** COULD NOT FIND RUBY GEMS REQUIRED BY SONIC PI ***"
  STDERR.puts "Directory '#{ruby_gem_api_path}' not found."
  STDERR.puts "Your ruby interpreter is '#{RbConfig.ruby}', supporting ruby api #{ruby_api}."
  Dir.entries("#{ruby_gem_native_path}/#{os}/")
    .select { |d| (File.directory?("#{ruby_gem_native_path}/#{os}/#{d}") && d != '.' && d != '..') }
    .each do |installed_ruby_api|
      STDERR.puts "The Sonic Pi on your computer was installed for ruby api #{installed_ruby_api}."
    end
  STDERR.puts "Please refer to the Sonic Pi install instructions."
  STDERR.puts "For installation, you need to run 'app/server/bin/compile-extensions.rb'."
  STDERR.puts "If you change or upgrade your ruby interpreter later, you may need to run it again."

  raise "Could not access ruby gem directory"
end

$:.unshift ruby_gem_api_path

require 'win32/process' if os == :windows

## Ensure all libs in vendor directory are available
Dir["#{File.expand_path("../vendor", __FILE__)}/*/lib/"].each do |vendor_lib|
  $:.unshift vendor_lib
end

begin
  require 'did_you_mean'
rescue LoadError
  warn "Non-critical error: Could not load did_you_mean"
end

require 'hamster/vector'
require 'wavefile'

module SonicPi
  module Core
    module SPRand
      # use FHS directory scheme:
      # check if Sonic Pi's ruby server is not running inside the
      # user's home directory, but is installed in /usr/lib/sonic-pi
      # on Linux from a distribution's package
      random_numbers_path = File.dirname(__FILE__).start_with?("/usr/lib/sonic-pi") ? "/usr/share/sonic-pi" : "../../../etc"
      # Read in same random numbers as server for random stream sync
      @@random_numbers = ::WaveFile::Reader.new(File.expand_path("#{random_numbers_path}/buffers/rand-stream.wav", __FILE__), ::WaveFile::Format.new(:mono, :float, 44100)).read(441000).samples.freeze

      def self.to_a
        @@random_numbers
      end

      def self.inc_idx!(increment=1, init=0)
        ridx = Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_idx) || init
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, ridx + increment
        ridx
      end

      def self.dec_idx!(decrement=1, init=0)
        ridx = Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_idx) || init
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, ridx - decrement
        ridx
      end

      def self.set_seed!(seed, idx=0)
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_seed, seed
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, idx
      end

      def self.set_idx!(idx)
        Thread.current.thread_variable_set :sonic_pi_spider_random_gen_idx, idx
      end

      def self.get_seed_and_idx
        [Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_seed),
          Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_idx)]
      end

      def self.get_seed
        Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_seed) || 0
      end

      def self.get_idx
        Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_idx) || 0
      end

      def self.get_seed_plus_idx
        (Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_idx) || 0) +
          Thread.current.thread_variable_get(:sonic_pi_spider_random_gen_seed) || 0
      end

      def self.rand!(max=1, idx=nil)
        idx = inc_idx! unless idx
        rand_peek(max, idx)
      end

      def self.rand_peek(max=1, idx=nil, seed=nil)
        idx = get_idx unless idx
        seed = get_seed unless seed
        idx = seed + idx
        # we know that the fixed rand stream has length 441000
        # also, scsynth server seems to swallow first rand
        # so always add 1 to index
        idx = (idx + 1) % 441000
        @@random_numbers[idx] * max
      end

      def self.rand_i!(max, idx=nil)
        rand!(max, idx).to_i
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
        super
      end

      def ___sp_vector_name
        "vector"
      end

      def ___sp_preserve_vec_kind(a)
        self.class.new(a)
      end

      def list_diff(other)
        ___sp_preserve_vec_kind(self.to_a - other.to_a)
      end

      def list_concat(other)
        ___sp_preserve_vec_kind(self.to_a + other.to_a)
      end

      def -(other)
        if other.is_a?(Array) || other.is_a?(SPVector)
          return list_diff(other)
        else
          o = other.to_f
          return self.map{|el| el - o}
        end
      end

      def +(other)
        if other.is_a?(Array) || other.is_a?(SPVector)
          return list_concat(other)
        else
          o = other.to_f
          return self.map{|el| el + o}
        end
      end

      def [](idx, len=(missing_length = true))
        return nil unless idx
        raise InvalidIndexError, "Invalid index: #{idx.inspect}, was expecting a number or range" unless idx && (idx.is_a?(Numeric) || idx.is_a?(Range))
        return nil if self.empty?
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
        self[SonicPi::Core::SPRand.rand_i!(self.size)]
      end

      def sample
        choose
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

      def reflect(n=1)
        res = self + self.reverse.drop(1)
        res = res + (res.drop(1) * (n - 1)) if n > 1
        res
      end

      def mirror(n=1)
        (self + self.reverse) * n
      end

      def repeat(n=2)
        n = 1 if n < 1
        self * n
      end

      def drop_last(n=1)
        self[0...(size-n)]
      end

      def take_last(n=1)
        self if n >= size
        self[(size-n)..-1]
      end

      def butlast
        drop_last(1)
      end

      def take(n)
        return self.reverse.take(-n) if n <= 0
        return super if n <= @size
        self + take(n - @size)
      end

      def pick(n=nil, *opts)
        # mangle args to extract nice behaviour
        if !n.is_a?(Numeric) && opts.empty?
          opts = n
          n = nil
        else
          opts = opts[0]
        end

        if opts.is_a?(Hash)
          s = opts[:skip]
        else
          s = nil
        end

        n = @size unless n
        raise "pick requires n to be a number, got: #{n.inspect}" unless n.is_a? Numeric

        res = []
        if s
          raise "skip: opt needs to be a number, got: #{s.inspect}" unless s.is_a? Numeric
          n.times do
            SonicPi::Core::SPRand.inc_idx!(s)
            res << self.choose
          end
        else
          n.times do
            res << self.choose
          end
        end
        res.ring
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

      def map_index(idx)
        idx
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
    self[SonicPi::Core::SPRand.rand_i!(self.size)]
  end

  def pick(n=nil)
    n = @size unless n
    raise "pick requires n to be a number, got: #{n.inspect}" unless n.is_a? Numeric

    res = []
    n.times do
      res << self.choose
    end
    res
  end

  alias_method :__orig_sample__, :sample
  def sample(*args, &blk)

    if Thread.current.thread_variable_get(:sonic_pi_spider_thread)
      self[SonicPi::Core::SPRand.rand!(self.size)]
    else
      __orig_sample__ *args, &blk
    end
  end

  alias_method :__orig_shuffle__, :shuffle
  def shuffle(*args, &blk)
    if Thread.current.thread_variable_get(:sonic_pi_spider_thread)
      orig_seed, orig_idx = SonicPi::Core::SPRand.get_seed_and_idx
      SonicPi::Core::SPRand.set_seed!(SonicPi::Core::SPRand.rand_i!(441000))
      new_a = self.dup
      s = new_a.size
      s.times do
        idx_a = SonicPi::Core::SPRand.rand!(s)
        idx_b = SonicPi::Core::SPRand.rand!(s)
        new_a[idx_a], new_a[idx_b] = new_a[idx_b], new_a[idx_a]
      end
      SonicPi::Core::SPRand.set_seed!(orig_seed, orig_idx + 1)
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
