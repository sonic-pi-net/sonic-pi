require "forwardable"
require "hamster/immutable"
require "hamster/undefined"
require "hamster/enumerable"
require "hamster/trie"
require "hamster/sorted_set"

module Hamster
  def self.set(*items)
    items.empty? ? EmptySet : Set.new(items)
  end

  # `Hamster::Set` is a collection of unordered values with no duplicates. Testing whether
  # an object is present in the `Set` is fast. `Set` is also `Enumerable`, so you can
  # iterate over the members of the set with {#each}, transform them with {#map}, filter
  # them with {#select}, and so on. Some of the `Enumerable` methods are overridden to
  # return Hamster collections.
  #
  # Like the `Set` class in Ruby's standard library, which we will call RubySet,
  # `Hamster::Set` defines equivalency of objects using `#hash` and `#eql?`. No two
  # objects with the same `#hash` code, and which are also `#eql?`, can coexist in the
  # same `Set`. If one is already in the `Set`, attempts to add another one will have
  # no effect.
  #
  # `Set`s have no natural ordering and cannot be compared using `#<=>`. However, they
  # define {#<}, {#>}, {#<=}, and {#>=} as shorthand for {#proper_subset?},
  # {#proper_superset?}, {#subset?}, and {#superset?} (respectively).
  #
  # The basic set-theoretic operations {#union}, {#intersection}, {#difference}, and
  # {#exclusion} work with any `Enumerable` object. They may be more efficient when used
  # with another `Hamster::Set`, or a RubySet.
  #
  # A `Set` can be created in any of the following ways:
  #
  #     Hamster.set('Tom', 'Dick', 'Harry')
  #     Hamster::Set.new([1, 2, 3]) # any Enumerable can be used to initialize
  #     Hamster::Set['A', 'B', 'C', 'D']
  #
  # The latter 2 forms of initialization can be used with your own, custom subclasses
  # of `Hamster::Set`.
  #
  # Unlike RubySet, all methods which you might expect to "modify" a `Hamster::Set`
  # actually return a new set and leave the existing one unchanged.
  #
  # @example
  #   require 'hamster/set'
  #   set1 = Hamster.set(1, 2)  # => Hamster::Set[1, 2]
  #   set2 = Hamster::Set[1, 2] # => Hamster::Set[1, 2]
  #   set1 == set2              # => true
  #   set3 = set1.add("foo")    # => Hamster::Set[1, 2, "foo"]
  #   set3 - set2               # => Hamster::Set["foo"]
  #   set3.subset?(set1)        # => false
  #   set1.subset?(set3)        # => true
  #
  class Set
    extend Forwardable
    include Immutable
    include Enumerable

    class << self
      # Create a new `Set` populated with the given items.
      # @return [Set]
      def [](*items)
        new(items)
      end

      # Return an empty `Set`. If used on a subclass, returns an empty instance
      # of that class.
      #
      # @return [Set]
      def empty
        @empty ||= self.new
      end

      # "Raw" allocation of a new `Set`. Used internally to create a new
      # instance quickly after obtaining a modified {Trie}.
      #
      # @return [Set]
      # @private
      def alloc(trie = EmptyTrie)
        allocate.tap { |s| s.instance_variable_set(:@trie, trie) }
      end
    end

    def initialize(items=[])
      @trie = Trie.new(0)
      items.each { |item| @trie.put!(item, nil) }
    end

    # Return `true` if this `Set` contains no items.
    # @return [Boolean]
    def empty?
      @trie.empty?
    end
    def_delegator :self, :empty?, :null?

    # Return the number of items in this `Set`.
    # @return [Integer]
    def size
      @trie.size
    end
    def_delegator :self, :size, :length

    # Return a new `Set` with `item` added. If `item` is already in the set,
    # return `self`.
    #
    # @param item [Object] The object to add
    # @return [Set]
    def add(item)
      include?(item) ? self : self.class.alloc(@trie.put(item, nil))
    end
    def_delegator :self, :add, :<<
    def_delegator :self, :add, :conj
    def_delegator :self, :add, :conjoin

    # If `item` is not a member of this `Set`, return a new `Set` with `item` added.
    # Otherwise, return `false`.
    #
    # @param item [Object] The object to add
    # @return [Set, false]
    def add?(item)
      !include?(item) && add(item)
    end

    # Return a new `Set` with `item` removed. If `item` is not a member of the set,
    # return `self`.
    #
    # @param item [Object] The object to remove
    # @return [Set]
    def delete(item)
      trie = @trie.delete(item)
      if trie.equal?(@trie)
        self
      elsif trie.empty?
        self.class.empty
      else
        self.class.alloc(trie)
      end
    end

    # If `item` is a member of this `Set`, return a new `Set` with `item` removed.
    # Otherwise, return `false`.
    #
    # @param item [Object] The object to remove
    # @return [Set, false]
    def delete?(item)
      include?(item) && delete(item)
    end

    # Call the block once for each item in this `Set`. No specific iteration order
    # is guaranteed (but the order will be stable for any particular `Set`.)
    #
    # @return [self]
    def each
      return to_enum if not block_given?
      @trie.each { |key, _| yield(key) }
      self
    end

    # Call the block once for each item in this `Set`. Iteration order will be
    # the opposite of {#each}.
    #
    # @return [self]
    def reverse_each
      return enum_for(:reverse_each) if not block_given?
      @trie.reverse_each { |key, _| yield(key) }
      self
    end

    # Return a new `Set` with all the items for which the block returns true.
    #
    # @return [Set]
    def filter
      return enum_for(:filter) unless block_given?
      trie = @trie.filter { |entry| yield(entry[0]) }
      return self.class.empty if trie.empty?
      trie.equal?(@trie) ? self : self.class.alloc(trie)
    end

    def_delegator :self, :reduce, :foldr # set is not ordered, so foldr is same as reduce

    # Call the block once for each item in this `Set`.
    # All the values returned from the block will be gathered into a new `Set`.
    #
    # @return [Set]
    def map
      return enum_for(:map) if not block_given?
      return self if empty?
      self.class.new(super)
    end
    def_delegator :self, :map, :collect

    # Return `true` if the given item is present in this `Set`. More precisely,
    # return `true` if an object with the same `#hash` code, and which is also `#eql?`
    # to the given object is present.
    #
    # @param object [Object] The object to check for
    # @return [Boolean]
    def include?(object)
      @trie.key?(object)
    end
    def_delegator :self, :include?, :member?

    # Return a member of this `Set`. The member chosen will be the first one which
    # would be yielded by {#each}. If the set is empty, return `nil`.
    #
    # @return [Object]
    def first
      (entry = @trie.at(0)) && entry[0]
    end
    def_delegator :self, :first, :head

    # Return a {SortedSet} which contains the same items as this `Set`, ordered by
    # the given comparator block. The comparator block should take 2 parameters and
    # return 0, 1, or -1 depending on whether the first parameter is equal, greater than,
    # or less than the second.
    #
    # @yield [a, b] A pair of items to be compared
    # @yieldreturn [Integer]
    # @return [SortedSet]
    def sort(&comparator)
      SortedSet.new(self.to_a, &comparator)
    end

    # Return a {SortedSet} which contains the same items as this `Set`, ordered by
    # mapping each item through the provided block to obtain sort keys, and then
    # sorting the keys.
    #
    # @yield [item] The item to obtain a sort key for
    # @yieldreturn [Object]
    # @return [SortedSet]
    def sort_by(&mapper)
      SortedSet.new(self.to_a, &mapper)
    end

    # Return a new `Set` which contains all the members of both this `Set` and `other`.
    # `other` can be any `Enumerable` object.
    #
    # @example
    #   Hamster::Set[1, 2] | Hamster::Set[2, 3] # => Hamster::Set[1, 2, 3]
    #
    # @param other [Enumerable] The collection to merge with
    # @return [Set]
    def union(other)
      trie = other.reduce(@trie) do |a, element|
        next a if a.key?(element)
        a.put(element, nil)
      end
      trie.equal?(@trie) ? self : self.class.alloc(trie)
    end
    def_delegator :self, :union, :|
    def_delegator :self, :union, :+
    def_delegator :self, :union, :merge

    # Return a new `Set` which contains all the items which are members of both
    # this `Set` and `other`. `other` can be any `Enumerable` object.
    #
    # @example
    #   Hamster::Set[1, 2] & Hamster::Set[2, 3] # => Hamster::Set[2]
    #
    # @param other [Enumerable] The collection to intersect with
    # @return [Set]
    def intersection(other)
      if (other.size < @trie.size) && other.is_a?(Hamster::Set)
        trie = other.instance_variable_get(:@trie).filter { |key, _| include?(key) }
      else
        trie = @trie.filter { |key, _| other.include?(key) }
      end
      trie.equal?(@trie) ? self : self.class.alloc(trie)
    end
    def_delegator :self, :intersection, :intersect
    def_delegator :self, :intersection, :&

    # Return a new `Set` with all the items in `other` removed. `other` can be
    # any `Enumerable` object.
    #
    # @example
    #   Hamster::Set[1, 2] - Hamster::Set[2, 3] # => Hamster::Set[1]
    #
    # @param other [Enumerable] The collection to subtract from this set
    # @return [Set]
    def difference(other)
      trie = if (@trie.size <= other.size) && (other.is_a?(Hamster::Set) || (defined?(::Set) && other.is_a?(::Set)))
        @trie.filter { |key, _| !other.include?(key) }
      else
        other.reduce(@trie) { |trie, item| trie.delete(item) }
      end
      trie.empty? ? self.class.empty : self.class.alloc(trie)
    end
    def_delegator :self, :difference, :diff
    def_delegator :self, :difference, :subtract
    def_delegator :self, :difference, :-

    # Return a new `Set` which contains all the items which are members of this
    # `Set` or of `other`, but not both. `other` can be any `Enumerable` object.
    #
    # @example
    #   Hamster::Set[1, 2] ^ Hamster::Set[2, 3] # => Hamster::Set[1, 3]
    #
    # @param other [Enumerable] The collection to take the exclusive disjunction of
    # @return [Set]
    def exclusion(other)
      ((self | other) - (self & other))
    end
    def_delegator :self, :exclusion, :^

    # Return `true` if all items in this `Set` are also in `other`.
    #
    # @param other [Set]
    # @return [Boolean]
    def subset?(other)
      return false if other.size < size
      all? { |item| other.include?(item) }
    end
    alias :<= :subset?

    # Return `true` if all items in `other` are also in this `Set`.
    #
    # @param other [Set]
    # @return [Boolean]
    def superset?(other)
      other.subset?(self)
    end
    alias :>= :superset?

    # Returns `true` if `other` contains all the items in this `Set`, plus at least
    # one item which is not in this set.
    #
    # @param other [Set]
    # @return [Boolean]
    def proper_subset?(other)
      return false if other.size <= size
      all? { |item| other.include?(item) }
    end
    alias :< :proper_subset?

    # Returns `true` if this `Set` contains all the items in `other`, plus at least
    # one item which is not in `other`.
    #
    # @param other [Set]
    # @return [Boolean]
    def proper_superset?(other)
      other.proper_subset?(self)
    end
    alias :> :proper_superset?

    # Return `true` if this `Set` and `other` do not share any items.
    #
    # @param other [Set]
    # @return [Boolean]
    def disjoint?(other)
      if size < other.size
        each { |item| return false if other.include?(item) }
      else
        other.each { |item| return false if include?(item) }
      end
      true
    end

    # Return `true` if this `Set` and `other` have at least one item in common.
    #
    # @param other [Set]
    # @return [Boolean]
    def intersect?(other)
      !disjoint?(other)
    end

    # Recursively insert the contents of any nested `Set`s into this `Set`, and
    # remove them.
    #
    # @example
    #   Hamster::Set[Hamster::Set[1, 2], Hamster::Set[3, 4]].flatten
    #   # => Hamster::Set[1, 2, 3, 4]
    #
    # @return [Set]
    def flatten
      reduce(self.class.empty) do |set, item|
        next set.union(item.flatten) if item.is_a?(Set)
        set.add(item)
      end
    end

    def_delegator :self, :group_by, :group
    def_delegator :self, :group_by, :classify

    # Return a randomly chosen item from this `Set`. If the set is empty, return `nil`.
    #
    # @return [Object]
    def sample
      empty? ? nil : @trie.at(rand(size))[0]
    end

    # Return an empty `Set` instance, of the same class as this one. Useful if you
    # have multiple subclasses of `Set` and want to treat them polymorphically.
    #
    # @return [Hash]
    def clear
      self.class.empty
    end

    # Return true if `other` has the same type and contents as this `Set`.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean]
    def eql?(other)
      return true if other.equal?(self)
      return false if not instance_of?(other.class)
      other_trie = other.instance_variable_get(:@trie)
      return false if @trie.size != other_trie.size
      @trie.each do |key, _|
        return false if !other_trie.key?(key)
      end
      true
    end
    def_delegator :self, :eql?, :==

    # See `Object#hash`.
    # @return [Integer]
    def hash
      reduce(0) { |hash, item| (hash << 5) - hash + item.hash }
    end

    undef :"<=>" # Sets are not ordered, so Enumerable#<=> will give a meaningless result
    undef :each_index # Set members cannot be accessed by 'index', so #each_index is not meaningful

    def_delegator :self, :dup, :uniq
    def_delegator :self, :dup, :nub
    def_delegator :self, :dup, :remove_duplicates

    # Return `self`.
    #
    # @return [self]
    def to_set
      self
    end

    # @private
    def marshal_dump
      output = {}
      each do |key|
        output[key] = nil
      end
      output
    end

    # @private
    def marshal_load(dictionary)
      @trie = dictionary.reduce(EmptyTrie) do |trie, key_value|
        trie.put(key_value.first, nil)
      end
    end
  end

  # The canonical empty `Set`. Returned by `Hamster.set` and `Set[]` when
  # invoked with no arguments; also returned by `Set.empty`. Prefer using this
  # one rather than creating many empty sets using `Set.new`.
  #
  EmptySet = Hamster::Set.empty
end
