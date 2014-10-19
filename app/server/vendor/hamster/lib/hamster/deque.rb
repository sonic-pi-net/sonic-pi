require "forwardable"
require "hamster/immutable"
require "hamster/list"

module Hamster
  def self.deque(*items)
    items.empty? ? EmptyDeque : Deque.new(items)
  end

  # A `Deque` (or double-ended queue) is an ordered, sequential collection of objects,
  # which allows elements to be efficiently added and removed at the front and end of
  # the sequence. Retrieving the elements at the front and end is also efficient. This
  # makes `Deque` perfect for use as an immutable queue *or* stack.
  #
  # A `Deque` differs from a {Vector} in that vectors allow indexed access to any
  # element in the collection. `Deque`s only allow access to the first and last
  # element. But adding and removing from the ends of a `Deque` is faster than
  # adding and removing from the ends of a {Vector}.
  #
  # To create a new `Deque`:
  #
  #     Hamster.deque('a', 'b', 'c')
  #     Hamster::Deque.new([:first, :second, :third])
  #     Hamster::Deque[1, 2, 3, 4, 5]
  #
  # Or you can start with an empty deque and build it up:
  #
  #     Hamster::Deque.empty.push('b').push('c').unshift('a')
  #
  # Like all Hamster collections, `Deque` is immutable. The 4 basic operations which
  # "modify" deques ({#push}, {#pop}, {#shift}, and {#unshift}) all return a new
  # collection and leave the existing one unchanged.
  #
  # @example
  #   deque = Hamster::Deque.empty                 # => Hamster::Deque[]
  #   deque = deque.push('a').push('b').push('c')  # => Hamster::Deque['a', 'b', 'c']
  #   deque.first                                  # => 'a'
  #   deque.last                                   # => 'c'
  #   deque = deque.shift                          # => Hamster::Deque['b', 'c']
  #
  # @see http://en.wikipedia.org/wiki/Deque "Deque" on Wikipedia
  #
  class Deque
    extend Forwardable
    include Immutable

    class << self
      # Create a new `Deque` populated with the given items.
      # @return [Deque]
      def [](*items)
        items.empty? ? empty : new(items)
      end

      # Return an empty `Deque`. If used on a subclass, returns an empty instance
      # of that class.
      #
      # @return [Deque]
      def empty
        @empty ||= self.new
      end

      # "Raw" allocation of a new `Deque`. Used internally to create a new
      # instance quickly after consing onto the front/rear lists or taking their
      # tails.
      #
      # @return [Deque]
      # @private
      def alloc(front, rear)
        result = allocate
        result.instance_variable_set(:@front, front)
        result.instance_variable_set(:@rear,  rear)
        result
      end
    end

    def initialize(items=[])
      @front = items.to_list
      @rear  = EmptyList
    end

    # Return `true` if this `Deque` contains no items.
    # @return [Boolean]
    def empty?
      @front.empty? && @rear.empty?
    end
    def_delegator :self, :empty?, :null?

    # Return the number of items in this `Deque`.
    # @return [Integer]
    def size
      @front.size + @rear.size
    end
    def_delegator :self, :size, :length

    # Return the first item in the `Deque`. If the deque is empty, return `nil`.
    # @return [Object]
    def first
      return @front.head unless @front.empty?
      @rear.last # memoize?
    end
    def_delegator :self, :first, :head
    def_delegator :self, :first, :front

    # Return the last item in the `Deque`. If the deque is empty, return `nil`.
    # @return [Object]
    def last
      return @rear.head unless @rear.empty?
      @front.last # memoize?
    end
    def_delegator :self, :last, :peek

    # Return a new `Deque` with `item` added at the end.
    # @param item [Object] The item to add
    # @return [Deque]
    def push(item)
      self.class.alloc(@front, @rear.cons(item))
    end
    def_delegator :self, :push, :enqueue
    def_delegator :self, :push, :<<
    def_delegator :self, :push, :add
    def_delegator :self, :push, :conj
    def_delegator :self, :push, :conjoin

    # Return a new `Deque` with the last item removed.
    # @return [Deque]
    def pop
      front, rear = @front, @rear

      if rear.empty?
        return EmptyDeque if front.empty?
        front, rear = EmptyList, front.reverse
      end

      self.class.alloc(front, rear.tail)
    end

    # Return a new `Deque` with `item` added at the front.
    # @param item [Object] The item to add
    # @return [Deque]
    def unshift(item)
      self.class.alloc(@front.cons(item), @rear)
    end

    # Return a new `Deque` with the first item removed.
    # @return [Deque]
    def shift
      front, rear = @front, @rear

      if front.empty?
        return EmptyDeque if rear.empty?
        front, rear = rear.reverse, EmptyList
      end

      self.class.alloc(front.tail, rear)
    end
    def_delegator :self, :shift, :dequeue
    def_delegator :self, :shift, :tail

    # Return an empty `Deque` instance, of the same class as this one. Useful if you
    # have multiple subclasses of `Deque` and want to treat them polymorphically.
    #
    # @return [Deque]
    def clear
      self.class.empty
    end

    # Return true if `other` has the same type and contents as this `Deque`.
    #
    # @param other [Object] The collection to compare with
    # @return [Boolean]
    def eql?(other)
      return true if other.equal?(self)
      instance_of?(other.class) && to_ary.eql?(other.to_ary)
    end
    def_delegator :self, :eql?, :==

    # Return an `Array` with the same elements, in the same order.
    # @return [Array]
    def to_a
      @front.to_a.concat(@rear.to_a.tap { |a| a.reverse! })
    end
    def_delegator :self, :to_a, :entries
    def_delegator :self, :to_a, :to_ary

    # Return a {List} with the same elements, in the same order.
    # @return [Hamster::List]
    def to_list
      @front.append(@rear.reverse)
    end

    # Return the contents of this `Deque` as a programmer-readable `String`. If all the
    # items in the deque are serializable as Ruby literal strings, the returned string can
    # be passed to `eval` to reconstitute an equivalent `Deque`.
    #
    # @return [String]
    def inspect
      result = "#{self.class}["
      i = 0
      @front.each { |obj| result << ', ' if i > 0; result << obj.inspect; i += 1 }
      @rear.to_a.tap { |a| a.reverse! }.each { |obj| result << ', ' if i > 0; result << obj.inspect; i += 1 }
      result << "]"
    end

    # @private
    def pretty_print(pp)
      pp.group(1, "#{self.class}[", "]") do
        pp.breakable ''
        pp.seplist(self.to_a) { |obj| obj.pretty_print(pp) }
      end
    end

    # @return [::Array]
    # @private
    def marshal_dump
      to_a
    end

    # @private
    def marshal_load(array)
      initialize(array)
    end
  end

  # The canonical empty `Deque`. Returned by `Hamster.deque` and `Deque[]` when
  # invoked with no arguments; also returned by `Deque.empty`. Prefer using this
  # one rather than creating many empty deques using `Deque.new`.
  #
  EmptyDeque = Hamster::Deque.empty
end