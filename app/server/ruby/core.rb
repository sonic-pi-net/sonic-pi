#!/usr/bin/env ruby
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

## This core file sets up the load path and applies any necessary monkeypatches.

raise "Sonic Pi requires Ruby 2.4+ to be installed. You are using version #{RUBY_VERSION}" if RUBY_VERSION < "2.4"

## Ensure native lib dir is available
require 'rbconfig'
ruby_api = RbConfig::CONFIG['ruby_version']


## Ensure all libs in vendor directory are available
Dir["#{File.expand_path("../vendor", __FILE__)}/*/lib/"].each do |vendor_lib|
  $:.unshift vendor_lib
end

begin
  require 'did_you_mean'
rescue LoadError
  warn "Non-critical error: Could not load did_you_mean"
end

require 'wavefile'

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


$:.unshift "#{File.expand_path("../rb-native", __FILE__)}/#{ruby_api}/"

## Add aubio native library to ENV if not present (the aubio library needs to be told the location)
native_lib_path = File.expand_path("../../native/", __FILE__)
ENV["AUBIO_LIB"] ||= Dir[native_lib_path + "/lib/libaubio*.{*dylib,so*,dll}"].first

module SonicPi
  module Core
    class ThreadLocal
      attr_reader :vars, :local_vars

      def initialize(parent=nil, overrides=nil)
        raise "ThreadLocal can only be initialized with nil or a parent ThreadLocal" unless parent.nil? || parent.is_a?(ThreadLocal)
        @parent_visible = true

        if parent && !parent.vars.empty?
          if overrides
            @parent_vars = parent.vars.merge(overrides)
            @vars = parent.vars.merge(overrides)
          else
            @parent_vars = parent.vars.dup
            @vars = parent.vars.dup
          end
        else
          overrides = {} unless overrides
          @parent_vars = overrides
          @vars = overrides
        end

        @local_vars = {}
      end

      def to_s
        "<ThreadLocal @vars: #{@vars}, @local_vars: #{@local_vars}"
      end

      def set(name, val)
        raise "Error setting Thread Local - value must be immutable. Got: #{val.inspect} for #{name.inspect}" unless val.sp_thread_safe?
        @vars[name] = val
        @local_vars.delete name
        val
      end

      def reset!
        @parent_visible = true
        @vars = @parent_vars.clone
        @local_vars = {}
      end

      def clear!
        @parent_visible = false
        @vars = {}
        @local_vars = {}
      end

      # These values will not be inherited
      def set_local(name, val)
        @local_vars[name] = val
        @vars.delete name
        val
      end

      def get(name, default=nil)
        if @local_vars.has_key? name
          return @local_vars[name]
        elsif @vars.has_key? name
          return @vars[name]
        elsif @parent_visible
          return @parent_vars.fetch(name, default)
        else
          return default
        end
      end

    end
  end
end



module SonicPi
  module Core
    module SPRand
      # we're limited to thread safe types so using ints
      NOISE_TYPES = {
        0 => :white,
        1 => :pink,
        2 => :light_pink,
        3 => :dark_pink,
        4 => :perlin
      }

      # define this helper first so we can set the constants below
      def self.wav_from_buffer_file(filename)
        ::WaveFile::Reader.new(
          File.expand_path("../../../../etc/buffers/#{filename}", __FILE__),
          ::WaveFile::Format.new(:mono, :float, 44100)
        ).read(441000).samples.freeze
      end

      # Read in same random numbers as server for random stream sync
      # memoize for performance
      RANDOM_NUMBERS_WHITE = self.wav_from_buffer_file("rand-stream.wav")
      RANDOM_NUMBERS_PINK = self.wav_from_buffer_file("rand-stream-pink.wav")
      RANDOM_NUMBERS_LIGHT_PINK = self.wav_from_buffer_file("rand-stream-light-pink.wav")
      RANDOM_NUMBERS_DARK_PINK = self.wav_from_buffer_file("rand-stream-dark-pink.wav")
      RANDOM_NUMBERS_PERLIN = self.wav_from_buffer_file("rand-stream-perlin.wav")

      def self.get_random_number_distribution
        type_idx = __thread_locals.get :sonic_pi_spider_random_gen_type
        NOISE_TYPES[type_idx]
      end

      def self.set_random_number_distribution!(noise_type)
        __thread_locals.set :sonic_pi_spider_random_gen_type, NOISE_TYPES.key(noise_type.to_sym)
      end

      def self.random_numbers
        idx = __thread_locals.get :sonic_pi_spider_random_gen_type
        case NOISE_TYPES[idx]
        when :perlin
          RANDOM_NUMBERS_PERLIN
        when :pink
          RANDOM_NUMBERS_PINK
        when :light_pink
          RANDOM_NUMBERS_LIGHT_PINK
        when :dark_pink
          RANDOM_NUMBERS_DARK_PINK
        when :white
          RANDOM_NUMBERS_WHITE
        else
          RANDOM_NUMBERS_WHITE
        end
      end

      def self.tl_seed_map(seed, idx=0)
        {:sonic_pi_spider_random_gen_seed => seed,
          :sonic_pi_spider_random_gen_idx => idx}
      end

      def self.__thread_locals(t = Thread.current)
        tls = t.thread_variable_get(:sonic_pi_thread_locals)
        tls = t.thread_variable_set(:sonic_pi_thread_locals, SonicPi::Core::ThreadLocal.new) unless tls
        return tls
      end

      def self.to_a
        random_numbers
      end

      def self.inc_idx!(increment=1, init=0)
        ridx = __thread_locals.get(:sonic_pi_spider_random_gen_idx, init)
        __thread_locals.set :sonic_pi_spider_random_gen_idx, ridx + increment
        ridx
      end

      def self.dec_idx!(decrement=1, init=0)
        ridx = __thread_locals.get(:sonic_pi_spider_random_gen_idx, init)
        __thread_locals.set :sonic_pi_spider_random_gen_idx, ridx - decrement
        ridx
      end

      def self.set_seed!(seed, idx=0)
        __thread_locals.set :sonic_pi_spider_random_gen_seed, seed
        __thread_locals.set :sonic_pi_spider_random_gen_idx, idx
      end

      def self.set_idx!(idx)
        __thread_locals.set :sonic_pi_spider_random_gen_idx, idx
      end

      def self.get_seed_and_idx
        [__thread_locals.get(:sonic_pi_spider_random_gen_seed),
          __thread_locals.get(:sonic_pi_spider_random_gen_idx)]
      end

      def self.get_seed
        __thread_locals.get(:sonic_pi_spider_random_gen_seed) || 0
      end

      def self.get_idx
        __thread_locals.get(:sonic_pi_spider_random_gen_idx) || 0
      end

      def self.get_seed_plus_idx
        (__thread_locals.get(:sonic_pi_spider_random_gen_idx) || 0) +
          __thread_locals.get(:sonic_pi_spider_random_gen_seed) || 0
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
        random_numbers[idx] * max
      end

      def self.rand_i!(max, idx=nil)
        rand!(max, idx).to_i
      end

    end

    module TLMixin
      def tick(*args)
        idx = SonicPi::Core::ThreadLocalCounter.tick(*args)
        self.ring[idx]
      end

      def look(*args)
        idx = SonicPi::Core::ThreadLocalCounter.look(*args)
        self.ring[idx]
      end
    end

    module ThreadLocalCounter

      def self.__thread_locals(t = Thread.current)
        tls = t.thread_variable_get(:sonic_pi_thread_locals)
        tls = t.thread_variable_set(:sonic_pi_thread_locals, SonicPi::Core::ThreadLocal.new) unless tls
        return tls
      end

      def self.get_or_create_counters
        counters = __thread_locals.get(:sonic_pi_local_core_thread_local_counters)
        return counters if counters
        counters = {}
        __thread_locals.set_local(:sonic_pi_local_core_thread_local_counters, counters)
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
          _, next_val = *counters[k]
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
        __thread_locals.set_local(:sonic_pi_local_core_thread_local_counters, {})
        nil
      end
    end
  end
end


module SonicPi
  module Core
    class EmptyVectorError < StandardError ; end
    class InvalidIndexError < StandardError ; end
    class NotThreadSafeError < StandardError ; end


    class SPMap
      attr_reader :map

      def initialize(*args)
        # let SPMaps be initialised from existing SPMaps and also standard Hashes.
        @map = args.length == 1 && args.first.is_a?(self.class) ? args.first.map : Hash[*args]
        @map.freeze
        res = @map.all? {|k, v| k.sp_thread_safe? && v.sp_thread_safe?}
        @sp_thread_safe = !!res
      end

      def sp_thread_safe?
        @sp_thread_safe
      end

      def sp_log_inspect
        if @map.empty?
          return "(map)"
        else
          s = String.new("(map ")
          longest_key = keys.to_a.map(&:to_s).map(&:size).sort[-1] + 2
          @map.each do |k, v|
            if k.is_a?(Symbol)
              pk = "#{k.to_s}:".ljust(longest_key)
              s << "#{pk} #{v.inspect},\n       "
            else
              s << "#{k.inspect.ljust(longest_key)} => #{v.inspect},\n       "
            end
          end
          s.strip!.chomp!(",")
          s << ")"
        end
        return s
      end

      def inspect
        if @map.empty?
          return "(map)"
        else
          s = String.new("(map ")
          @map.each do |k, v|
            if k.is_a?(Symbol)
              pk = "#{k.to_s}:"
              s << "#{pk} #{v.inspect}, "
            else
              s << "#{k.inspect} => #{v.inspect}, "
            end
          end
          s.chomp!(", ")
          s << ")"
        end
        return s
      end

      def <(other)
        @map < other.map
      end

      def <=(other)
        @map <= other.map
      end


      def ==(other)
        other.map == @map
      end

      def >(other)
        @map > other.map
      end

      def >=(other)
        @map >= other.map
      end

      def [](key)
        @map[key]
      end

      def any?(&block)
        @map.any?(&block)
      end

      def assoc(o)
        @map.assoc(o)
      end

      def compact
        self.class.new(@map.compact)
      end

      def dig(*keys)
        @map.dig(*keys)
      end

      def each(&block)
        @map.each(&block)
      end

      def each_key(&block)
        @map.each_key(&block)
      end

      def each_pair(&block)
        @map.each_pair(&block)
      end

      def empty?
        @map.empty?
      end

      def eql?(other)
        @map.eql?(other)
      end

      def fetch(*args, &block)
        @map.fetch(*args, &block)
      end

      def fetch_values(*args, &block)
        @map.fetch_values(*args, &block)
      end

      def filter(&block)
        self.class.new(@map.filter(&block))
      end

      def select(&block)
        self.class.new(@map.select(&block))
      end

      def flatten(*args)
        @map.flatten(*args)
      end

      def has_key?(k)
        @map.has_key?(k)
      end

      def has_value?(v)
        @map.has_value?(v)
      end

      def include?(k)
        @map.include?(k)
      end

      def invert
        self.class.new(@map.invert)
      end

      def key(v)
        @map.key(v)
      end

      def key?(v)
        @map.key?(v)
      end

      def keys
        SPVector.new(@map.keys)
      end

      def length
        @map.length
      end

      def size
        @map.size
      end

      def merge(*hs, &block)
        self.class.new(@map.merge(*hs, &block))
      end

      def rassoc(o)
        SPVector.new(@map.rassoc(o))
      end

      def reject(&block)
        self.class.new(@map.reject(&block))
      end

      def select(&block)
        self.class.new(@map.select(&block))
      end

      def slice(*keys)
        self.class.new(@map.slice(*keys))
      end

      def to_h
        @map.dup
      end

      def to_s
        inspect
      end

    end

    class SPVector
      include TLMixin
      attr_reader :vec

      def initialize(list)
        @vec = list
        @vec.freeze
        res = @vec.all? {|el| el.sp_thread_safe?}
        @thread_safe = !!res
      end

      def to_a
        @vec.dup
      end

      alias :to_ary :to_a

      def [](*idx)
        if idx.size == 1
          i = idx[0]
          case i
          when Range
            res = @vec[i.min, i.max]
            self.class.new(res) if res
          else
            @vec[map_index(i)]
          end
        else
          res = @vec[*idx]
          self.class.new(res) if res
        end
      end

      def *(i)
        self.class.new(@vec * i)
      end

      def &(v)
          self.class.new(@vec & v)
      end

      def +(other)
        case other
        when SPVector
          return self.class.new(@vec + other.vec)
        when Array
          return self.class.new(@vec + other)
        else
          o = other.to_f
          return self.class.new(@vec.map{|el| el + o})
        end
      end

      def -(other)
        case other
        when SPVector
          return self.class.new(@vec - other.vec)
        when Array
          return self.class.new(@vec - other)
        else
          o = other.to_f
          return self.class.new(@vec.map{|el| el - o})
        end
      end

      def <=>(other)
        @vec <=> other
      end

      def ==(other)
        other.class == self.class && other.vec == @vec
      end

      def all?(&block)
        @vec.all?(&block)
      end

      def any?(&block)
        @vec.any?(&block)
      end

      def compact
        self.class.new(@vec.compact)
      end

      def drop(n)
        self.class.new(@vec.drop(n))
      end

      def drop_last(n=1)
        self.class.new(@vec[0...(size-n)])
      end

      def each(&block)
        @vec.each(&block)
      end

      def each_with_index(&block)
        @vec.each_with_index(&block)
      end

      def empty?
        @vec.empty?
      end

      def eql?(other)
        other.class == self.class && @vec.eql?(other.vec)
      end

      def filter(&block)
        self.class.new(@vec.select(&block))
      end

      def first(n=nil)
        if n
          self.class.new(@vec.first(n))
        else
          @vec.first
        end
      end

      def flatten(*args)
        self.class.new(@vec.flatten(*args))
      end

      def flat_map(&block)
        self.class.new(@vec.flat_map(&block))
      end

      def index(*args, &block)
        @vec.index(*args, &block)
      end

      def join(*args)
        @vec.join(*args)
      end

      def last(n=nil)
        if n
          self.class.new(@vec.last(n))
        else
          @vec.last
        end
      end

      def length
        @vec.length
      end

      def map(&block)
        @vec.map(&block)
      end

      def max(n=nil, &block)
        if n
          self.class.new(@vec.max(n, &block))
        else
          @vec.max(&block)
        end
      end

      def min(n=nil, &block)
        if n
          self.class.new(@vec.min(n, &block))
        else
          @vec.min(&block)
        end
      end

      def reverse
        self.class.new(@vec.reverse)
      end

      def rotate(*args)
        self.class.new(@vec.rotate(*args))
      end

      def choose
        self[SonicPi::Core::SPRand.rand_i!(@vec.size)]
      end

      def sample
        choose
      end

      def size
        @vec.size
      end

      def shuffle
        self.class.new(@vec.shuffle)
      end

      def sort(&block)
        self.class.new(@vec.sort(&block))
      end

      def uniq(&block)
        self.class.new(@vec.uniq(&block))
      end

      def values_at(*args)
        self.class.new(@vec.values_at(*args))
      end

      def sp_thread_safe?
        return @thread_safe
      end

      def ___sp_vector_name
        "vector"
      end

      def __sp_make_thread_safe
        return self if @thread_safe
        self.class.new(map {|v| v.__sp_make_thread_safe })
      end

      def list_diff(other)
        self.class.new(@vec.to_a - other.to_a)
      end

      def list_concat(other)
        self.class.new(@vec.to_a + other.to_a)
      end

      def scale(val)
        val = val.to_f
        return self.class.new(@vec.map{|el| el * val})
      end

      def ring
        if is_a?(SonicPi::Core::SPVector)
          self
        else
          SonicPi::Core::RingVector.new(self)
        end
      end

      def ramp
        if is_a?(SonicPi::Core::RampVector)
          self
        else
          SonicPi::Core::RampVector.new(self)
        end
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

      def take_last(n=1)
        self if n >= size
        self[(size-n)..-1]
      end

      def butlast
        drop_last(1)
      end

      def take(n)
        return self.class.new([]) if n == 0
        return self.reverse.take(-n) if n < 0
        return self.class.new([]) if @vec.size < 1
        return self.class.new(@vec.take(n)) if n <= @vec.size
        self.class.new(@vec.cycle.take(n))
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

        n = 1 unless n
        raise "pick requires n to be a number, got: #{n.inspect}" unless n.is_a? Numeric

        res = []

        if s
          raise "skip: opt needs to be a number, got: #{s.inspect}" unless s.is_a? Numeric
          s.times do
            self.choose
          end
        end

        n.times { res << self.choose }

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

      def stretch(num_its)
        res = []
        self.each do |v|
          num_its.times do
            res << v
          end
        end
        self.class.new(res)
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


    # An immutable map which when used in a splat will
    # yield its vals
    class SPSplatMap < SPMap

      def initialize(h)
        raise "SPSplatMaps may only be initialised with a Hash. You supplied a #{h.class} with value #{h.inspect}" unless h.is_a?(Hash)
        super

        # as h is a standard Hash, the value insertion order is
        # preserved at this point. By sticking it into an array, we
        # guarantee preservation of this ofder after conversion to an
        # immutable Map
        @sp_orig_vals = h.values
      end

      def to_ary
        @sp_orig_vals.to_ary
      end
    end

  end
end

class Time
  def __sp_make_thread_safe
    return self if frozen?
    self.clone.freeze
  end
end


class String
  def sp_thread_safe?
    frozen?
  end

  def __sp_make_thread_safe
    return self if frozen?
    self.clone.freeze
  end

  def shuffle
    self.chars.to_a.shuffle.join
  end

  def ring
    self.chars.ring
  end
end

class Numeric
  # Upper bound - will clamp the value
  # to a value below max
  def max(other)
    return self if self <= other
    other
  end

  # Minimum bound - will clamp the value
  # to a value above min
  def min(other)
    return self if self >= other
    other
  end

  # Max and minimum bound - will clamp other
  # to a value below other and above -1 * other
  def clamp(other)
    self.max(other).min(other * -1)
  end
end

class Symbol
  def sp_thread_safe?
    true
  end

  def shuffle
    self.to_s.shuffle.to_sym
  end

  def ring
    self.to_s.ring
  end
end

class Float

  def sp_thread_safe?
    true
  end

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
      readable, _ = IO.select(@reading, @writing)

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



class Array
  include SonicPi::Core::TLMixin

  def __sp_make_thread_safe
    res = map {|v| v.__sp_make_thread_safe}
    res.freeze
  end

  def sp_thread_safe?
    frozen? && self.all? {|el| el.sp_thread_safe?}
  end

  def to_spv
    SonicPi::Core::SPVector.new(self)
  end

  def ring
    SonicPi::Core::RingVector.new(self)
  end

  def ramp
    SonicPi::Core::RampVector.new(self)
  end

  def choose
    self[SonicPi::Core::SPRand.rand_i!(self.size)]
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

    n = 1 unless n
    raise "pick requires n to be a number, got: #{n.inspect}" unless n.is_a? Numeric

    res = []
    if s
      raise "skip: opt needs to be a number, got: #{s.inspect}" unless s.is_a? Numeric
      s.times do
        self.choose
      end
    end

    n.times { res << self.choose }

    res
  end


  alias_method :__orig_sample__, :sample
  def sample(*args, &blk)

    if Thread.current.thread_variable_get(:sonic_pi_thread_locals)
      self[SonicPi::Core::SPRand.rand!(self.size)]
    else
      __orig_sample__(*args, &blk)
    end
  end

  alias_method :__orig_shuffle__, :shuffle
  def shuffle(*args, &blk)
    if Thread.current.thread_variable_get(:sonic_pi_thread_locals)
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
      __orig_shuffle__(*args, &blk)
    end
  end

  alias_method :__orig_shuffle_bang__, :shuffle!
  def shuffle!(*args, &blk)
    if Thread.current.thread_variable_get(:sonic_pi_thread_locals)
      new_a = self.shuffle
      self.replace(new_a)
    else
      __orig_shuffle_bang__(*args, &blk)
    end
  end
end

class Time
  def sp_thread_safe?
    frozen?
  end
end

class Hash
  def to_sp_map
    SonicPi::Core::SPMap.new(self)
  end

  def __sp_make_thread_safe
    res = {}
    each do |k, v|
      res[k.__sp_make_thread_safe] = v.__sp_make_thread_safe
    end
    res.to_sp_map
  end

  def sp_thread_safe?
    frozen? && all? {|k, v| k.sp_thread_safe? && v.sp_thread_safe?}
  end
end

class Object

  def sp_log_inspect
    inspect
  end

  def sp_thread_safe?
      self.is_a?(Numeric) ||
      self.is_a?(Symbol) ||
      self.is_a?(TrueClass) ||
      self.is_a?(FalseClass) ||
      self.is_a?(NilClass) ||
      (self.is_a?(SonicPi::Core::SPVector) && self.all? {|el| el.sp_thread_safe?})
  end

  def __sp_make_thread_safe
    return self if self.sp_thread_safe?

    raise SonicPi::Core::NotThreadSafeError, "Sorry, unable to make a #{self.class} thread safe"
  end


  def ring
    self.to_a.ring
  end

  def tick(*args)
    self.ring.tick(*args)
  end

  def look(*args)
    self.ring.look(*args)
  end

  # Meta-glasses from our hero Why to help us
  # see more clearly..

  # The hidden singleton lurks behind everyone
  def metaclass; class << self; self; end; end

  def meta_eval(&blk); metaclass.instance_eval(&blk); end
  # Adds methods to a metaclass
  def meta_def name, &blk
    meta_eval { define_method name, &blk }
  end
  # Defines an instance method within a class
  def class_def name, &blk
    class_eval { define_method name, &blk }
  end
end
