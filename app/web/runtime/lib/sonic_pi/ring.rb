# SPDX-License-Identifier: AGPL-3.0-or-later
# Sonic Pi's rings and ramps (its SPVector, RingVector and RampVector): an
# immutable list whose index wraps (a ring) or clamps (a ramp), with the
# transforms programs use on them. Method for method, what Sonic Pi has and
# nothing more, so a program that fails in Sonic Pi fails here too.
module SonicPi
  class Ring
    attr_reader :vec

    def initialize(a) = (@vec = a.to_a.dup.freeze)

    def ___sp_vector_name = "ring"
    def map_index(idx) = @vec.empty? ? idx : idx % @vec.size

    def to_a = @vec.dup
    alias_method :to_ary, :to_a

    def [](*idx)
      if idx.size == 1
        i = idx[0]
        if i.is_a?(Range)
          res = @vec[i.min, i.max]            # Sonic Pi's own reading: a start and a length
          res ? self.class.new(res) : nil
        else
          @vec[map_index(i)]
        end
      else
        res = @vec[*idx]
        res ? self.class.new(res) : nil
      end
    end

    def *(i) = self.class.new(@vec * i)
    def &(v) = self.class.new(@vec & v.to_a)

    def +(other)
      case other
      when Ring then self.class.new(@vec + other.vec)
      when Array then self.class.new(@vec + other)
      else
        o = other.to_f
        self.class.new(@vec.map { |el| el + o })
      end
    end

    def -(other)
      case other
      when Ring then self.class.new(@vec - other.vec)
      when Array then self.class.new(@vec - other)
      else
        o = other.to_f
        self.class.new(@vec.map { |el| el - o })
      end
    end

    def <=>(other) = @vec <=> other
    def ==(other) = other.class == self.class && other.vec == @vec
    def eql?(other) = other.class == self.class && @vec.eql?(other.vec)

    def all?(&b) = @vec.all?(&b)
    def any?(&b) = @vec.any?(&b)
    def compact = self.class.new(@vec.compact)
    def drop(n) = self.class.new(@vec.drop(n))
    def drop_last(n = 1) = self.class.new(@vec[0...(size - n)])
    def each(&b) = @vec.each(&b)
    def each_with_index(&b) = @vec.each_with_index(&b)
    def empty? = @vec.empty?
    def filter(&b) = self.class.new(@vec.select(&b))
    def first(n = nil) = n ? self.class.new(@vec.first(n)) : @vec.first
    def flatten(*args) = self.class.new(@vec.flatten(*args))
    def flat_map(&b) = self.class.new(@vec.flat_map(&b))
    def index(*args, &b) = @vec.index(*args, &b)
    def join(*args) = @vec.join(*args)
    def last(n = nil) = n ? self.class.new(@vec.last(n)) : @vec.last
    def length = @vec.length
    def size = @vec.size
    def map(&b) = self.class.new(@vec.map(&b))
    def max(n = nil, &b) = n ? self.class.new(@vec.max(n, &b)) : @vec.max(&b)
    def min(n = nil, &b) = n ? self.class.new(@vec.min(n, &b)) : @vec.min(&b)
    def reverse = self.class.new(@vec.reverse)
    def rotate(*args) = self.class.new(@vec.rotate(*args))
    def choose = self[SonicPi::Rand.current.rand_i!(@vec.size)]
    def sample = choose
    def shuffle = self.class.new(@vec.shuffle)
    # Ruby's other name for index, and membership: native's ring has both (note_range's pitches: asks include?)
    def find_index(*args, &block) = @vec.index(*args, &block)
    def include?(x) = @vec.include?(x)
    def sort(&b) = self.class.new(@vec.sort(&b))
    def uniq(&b) = self.class.new(@vec.uniq(&b))
    def values_at(*args) = self.class.new(@vec.values_at(*args))
    def list_diff(other) = self.class.new(@vec - other.to_a)
    def list_concat(other) = self.class.new(@vec + other.to_a)

    def scale(val)
      val = val.to_f
      self.class.new(@vec.map { |el| el * val })
    end

    def ring = is_a?(Ramp) ? Ring.new(@vec) : self
    def ramp = is_a?(Ramp) ? self : Ramp.new(@vec)

    def reflect(n = 1)
      res = self + reverse.drop(1)
      res = res + (res.drop(1) * (n - 1)) if n > 1
      res
    end

    def mirror(n = 1) = (self + reverse) * n

    def repeat(n = 2)
      n = 1 if n < 1
      self * n
    end

    def take_last(n = 1) = self[(size - n)..size - 1]
    def butlast = drop_last(1)

    def take(n)
      return self.class.new([]) if n == 0
      return reverse.take(-n) if n < 0
      return self.class.new([]) if @vec.size < 1
      return self.class.new(@vec.take(n)) if n <= @vec.size
      res = []
      n.times { |i| res << @vec[i % @vec.size] }
      self.class.new(res)
    end

    def pick(n = nil, *opts) = Ring.new(@vec.pick(n, *opts))

    def stretch(num_its)
      res = []
      @vec.each { |v| num_its.times { res << v } }
      self.class.new(res)
    end

    # tick and look count per thread; a ring picks with its counter
    def tick(*args) = self[SonicPi.current_lang.tick(*args)]
    def look(*args) = self[SonicPi.current_lang.look(*args)]

    def inspect = @vec.empty? ? "(#{___sp_vector_name})" : "(#{___sp_vector_name} #{@vec.map { |v| SonicPi.log_inspect(v) }.join(', ')})"
    def to_s = inspect
    def sp_log_inspect = inspect

    # native's western_theory.rb: each element as a note (a rest nil), and a melody turned upside down around a note
    # (ring :c4, :e4, :g4).invert_around(:c4) is (ring 60, 56, 53); rests pass through
    def notes(*args) = SonicPi::Ring.new(to_a.map { |n| SonicPi.current_lang.note(n, *args) })
    def invert_around(n)
      axis = SonicPi::Ring.new([n]).notes.first
      raise "Can only invert_around a note, not a rest" unless axis
      SonicPi::Ring.new(notes.to_a.map { |e| e && (2 * axis - e) })
    end
  end

  class Ramp < Ring
    def ___sp_vector_name = "ramp"

    def map_index(idx)
      idx = idx.to_i
      idx = [idx, size - 1].min
      [idx, 0].max
    end
  end

  # A string as Ruby prints it, with its UTF-8 kept as characters: mruby's
  # own inspect escapes every byte past ASCII.
  def self.str_inspect(s)
    out = "\""
    s.each_char do |c|
      out += case c
             when "\"" then "\\\""
             when "\\" then "\\\\"
             when "\n" then "\\n"
             when "\t" then "\\t"
             when "\r" then "\\r"
             when "\e" then "\\e"
             when "#" then "#"
             else c
             end
    end
    out + "\""
  end

  # What puts shows for a value.
  # a Hash as the Ruby Sonic Pi ships with (4.0) prints one, its values as Sonic Pi prints them: {a: 1, "k" => 2}
  def self.hash_key(k)
    return "#{k}: " if k.is_a?(Symbol) && k.to_s =~ /\A[A-Za-z_][A-Za-z0-9_]*[?!]?\z/
    return "#{str_inspect(k.to_s)}: " if k.is_a?(Symbol)
    "#{log_inspect(k)} => "
  end

  def self.log_inspect(v)
    case v
    when Float then FloatFormat.to_s(v)
    when String then str_inspect(v)
    when Array then "[#{v.map { |x| log_inspect(x) }.join(', ')}]"
    when Hash then v.empty? ? "{}" : "{#{v.map { |k, x| "#{hash_key(k)}#{log_inspect(x)}" }.join(', ')}}"
    when Ring then v.inspect
    else v.respond_to?(:sp_log_inspect) ? v.sp_log_inspect : v.inspect
    end
  end
end

# Sonic Pi's String: a ring of its characters (how patterns like "x--x" tick), and a shuffle.
class String
  def ring = SonicPi::Ring.new(chars)
  def shuffle = chars.shuffle.join
end

# And its Symbol, as native's core.rb has it: shuffled as its name is (Sonic
# Dreams shuffles its cue names), and a ring of its characters.
class Symbol
  def shuffle = to_s.shuffle.to_sym
  def ring = to_s.ring
end

class Array
  def ring = SonicPi::Ring.new(self)
  def ramp = SonicPi::Ramp.new(self)
  def choose = self[SonicPi::Rand.current.rand_i!(size)]
  def pick(n = nil, *opts)
    if !n.is_a?(Numeric) && opts.empty?
      opts = n
      n = nil
    else
      opts = opts[0]
    end
    s = opts.is_a?(Hash) ? opts[:skip] : nil
    n = 1 unless n
    raise "pick requires n to be a number, got: #{n.inspect}" unless n.is_a?(Numeric)
    if s
      raise "skip: opt needs to be a number, got: #{s.inspect}" unless s.is_a?(Numeric)
      s.times { choose }
    end
    res = []
    n.times { res << choose }
    res
  end
  alias_method :__plain_shuffle, :shuffle
  def shuffle = SonicPi::Rand.current ? SonicPi::Rand.current.shuffle(self) : __plain_shuffle
  # sample and shuffle! from Sonic Pi's random stream too, as native's core.rb has them: use_random_seed repeats them
  alias_method :__plain_sample, :sample if method_defined?(:sample)
  def sample(*args) = SonicPi::Rand.current ? self[SonicPi::Rand.current.rand!(size).to_i] : __plain_sample(*args)
  def shuffle! = replace(shuffle)
  # Sonic Pi's TLMixin: an array ticks like a ring
  def tick(*args) = ring[SonicPi.current_lang.tick(*args)]
  def look(*args) = ring[SonicPi.current_lang.look(*args)]
end

# What native's core.rb gives every object: a ring of it (its to_a), ticked and looked at as a ring is; and why's
# meta-glasses, which a program may reach for
class Object
  def ring = to_a.ring
  def tick(*args) = ring.tick(*args)
  def look(*args) = ring.look(*args)
  def metaclass = singleton_class
  def meta_eval(&blk) = metaclass.instance_eval(&blk)
  def meta_def(name, &blk) = meta_eval { define_method(name, &blk) }
  def class_def(name, &blk) = class_eval { define_method(name, &blk) }
end

# native's one-argument bounds: 5.max(3) is 3, 5.min(8) is 8, and clamp(n) keeps a value within -n and n (Ruby's
# two-argument clamp still works here)
class Numeric
  def max(other) = self <= other ? self : other
  def min(other) = self >= other ? self : other
  def clamp(lo, hi = nil) = hi.nil? ? max(lo).min(lo * -1) : (self < lo ? lo : self > hi ? hi : self)
end
