require "forwardable"
require "hamster/immutable"
require "hamster/enumerable"

module Hamster
  def self.sorted_set(*items, &block)
    (items.empty? && block.nil?) ? EmptySortedSet : SortedSet.new(items, &block)
  end

  # A `SortedSet` is a collection of ordered values with no duplicates. Unlike a
  # {Vector}, in which items can appear in any arbitrary order, a `SortedSet` always
  # keeps items either in their natural order, or in an order defined by a comparator
  # block which is provided at initialization time.
  #
  # `SortedSet` uses `#<=>` (or its comparator block) to determine which items are
  # equivalent. If the comparator indicates that an existing item and a new item are
  # equal, any attempt to insert the new item will have no effect.
  #
  # This means that *all* the items inserted into any one `SortedSet` must all be
  # comparable. For example, you cannot put `String`s and `Integer`s in the same
  # `SortedSet`. This is unlike {Set}, which can store items of any type, as long
  # as they all support `#hash` and `#eql?`.
  #
  # A `SortedSet` can be created in any of the following ways:
  #
  #     Hamster.sorted_set('Tom', 'Dick', 'Harry')
  #     Hamster::SortedSet.new([1, 2, 3]) # any Enumerable can be used to initialize
  #     Hamster::SortedSet['A', 'B', 'C', 'D']
  #
  # Or if you want to use a custom ordering:
  #
  #     Hamster.sorted_set('Tom', 'Dick', 'Harry') { |a, b| a.reverse <=> b.reverse }
  #     Hamster.sorted_set('Tom', 'Dick', 'Harry') { |str| str.reverse }
  #     Hamster::SortedSet.new([1,2,3]) { |a, b| -a <=> -b }
  #     Hamster::SortedSet.new([1, 2, 3]) { |num| -num }
  #
  # As you can see, `SortedSet` can use a 2-parameter block which returns 0, 1, or -1
  # as a comparator (like `Array#sort`), *or* use a 1-parameter block to derive sort
  # keys (like `Array#sort_by`) which will be compared using `#<=>`.
  #
  # Like all Hamster collections, `SortedSet`s are immutable. Any operation which you
  # might expect to "modify" a `SortedSet` will actually return a new collection and
  # leave the existing one unchanged.
  #
  # `SortedSet` supports the same basic set-theoretic operations as {Set}, including
  # {#union}, {#intersection}, {#difference}, and {#exclusion}, as well as {#subset?},
  # {#superset?}, and so on. Unlike {Set}, it does not define comparison operators like
  # {#>} or {#<} as aliases for the superset/subset predicates. Instead, these comparison
  # operators do a item-by-item comparison between the `SortedSet` and another sequential
  # collection. (See `Array#<=>` for details.)
  #
  # Additionally, since `SortedSet`s are ordered, they also support indexed retrieval
  # of items (or slices of items) using {#at} or {#[]}. Like {Vector} (or `Array`),
  # negative indices count back from the end of the `SortedSet`.
  #
  # Getting the {#max} or {#min} item from a `SortedSet`, as defined by its comparator,
  # is very efficient.
  #
  class SortedSet
    extend Forwardable
    include Immutable
    include Enumerable

    class << self
      # Create a new `SortedSet` populated with the given items. This method does not
      # accept a comparator block.
      #
      # @return [SortedSet]
      def [](*items)
        new(items)
      end

      # Return an empty `SortedSet`. If used on a subclass, returns an empty instance
      # of that class.
      #
      # @return [SortedSet]
      def empty
        @empty ||= self.alloc(EmptyAVLNode, lambda { |a,b| a <=> b })
      end

      # "Raw" allocation of a new `SortedSet`. Used internally to create a new
      # instance quickly after obtaining a modified binary tree.
      #
      # @return [Set]
      # @private
      def alloc(node, comparator)
        result = allocate
        result.instance_variable_set(:@node, node)
        result.instance_variable_set(:@comparator, comparator)
        result
      end
    end

    def initialize(items=[], &block)
      items = items.to_a
      if block
        @comparator = if block.arity == 1
          lambda { |a,b| block.call(a) <=> block.call(b) }
        else
          block
        end
        items = items.sort(&@comparator)
      else
        items = items.sort
      end
      @node = AVLNode.from_items(items, 0, items.size-1)
    end

    # Return `true` if this `SortedSet` contains no items.
    #
    # @return [Boolean]
    def empty?
      @node.empty?
    end
    def_delegator :self, :empty?, :null?

    # Return the number of items in this `SortedSet`.
    #
    # @return [Integer]
    def size
      @node.size
    end
    def_delegator :self, :size, :length

    # Return a new `SortedSet` with `item` added. If `item` is already in the set,
    # return `self`.
    #
    # @param item [Object] The object to add
    # @return [SortedSet]
    def add(item)
      return self if include?(item)
      node = @node.insert(item, @comparator)
      self.class.alloc(node, @comparator)
    end
    def_delegator :self, :add, :<<
    def_delegator :self, :add, :conj
    def_delegator :self, :add, :conjoin

    # If `item` is not a member of this `SortedSet`, return a new `SortedSet` with
    # `item` added. Otherwise, return `false`.
    #
    # @param item [Object] The object to add
    # @return [SortedSet, false]
    def add?(item)
      !include?(item) && add(item)
    end

    # Return a new `SortedSet` with `item` removed. If `item` is not a member of the set,
    # return `self`.
    #
    # @param item [Object] The object to remove
    # @return [SortedSet]
    def delete(item)
      return self if not include?(item)
      node = @node.delete(item, @comparator)
      if node.empty?
        self.class.empty
      else
        self.class.alloc(node, @comparator)
      end
    end

    # If `item` is a member of this `SortedSet`, return a new `SortedSet` with
    # `item` removed. Otherwise, return `false`.
    #
    # @param item [Object] The object to remove
    # @return [SortedSet, false]
    def delete?(item)
      include?(item) && delete(item)
    end

    # Return a new `SortedSet` with the item at `index` removed. If the given `index`
    # does not exist (if it is too high or too low), return `self`.
    #
    # @param index [Integer] The index to remove
    # @return [SortedSet]
    def delete_at(index)
      (item = at(index)) ? delete(item) : self
    end

    # Retrieve the item at `index`. If there is none (either the provided index
    # is too high or too low), return `nil`.
    #
    # @param index [Integer] The index to retrieve
    # @return [Object]
    def at(index)
      index += @node.size if index < 0
      return nil if index >= @node.size || index < 0
      @node.at(index)
    end

    # Retrieve the value at `index`, or use the provided default value or block,
    # or otherwise raise an `IndexError`.
    #
    # @overload fetch(index)
    #   Retrieve the value at the given index, or raise an `IndexError` if it is
    #   not found.
    #   @param index [Integer] The index to look up
    # @overload fetch(index) { |index| ... }
    #   Retrieve the value at the given index, or call the optional
    #   code block (with the non-existent index) and get its return value.
    #   @yield [index] The index which does not exist
    #   @yieldreturn [Object] Object to return instead
    #   @param index [Integer] The index to look up
    # @overload fetch(index, default)
    #   Retrieve the value at the given index, or else return the provided
    #   `default` value.
    #   @param index [Integer] The index to look up
    #   @param default [Object] Object to return if the key is not found
    #
    # @return [Object]
    def fetch(index, default = (missing_default = true))
      if index >= -@node.size && index < @node.size
        at(index)
      elsif block_given?
        yield(index)
      elsif !missing_default
        default
      else
        raise IndexError, "index #{index} outside of sorted set bounds"
      end
    end

    # Element reference. Return the item at a specific index, or a specified,
    # contiguous range of items (as a new `SortedSet`).
    #
    # @overload set[index]
    #   Return the item at `index`.
    #   @param index [Integer] The index to retrieve.
    # @overload set[start, length]
    #   Return a subset starting at index `start` and continuing for `length` elements.
    #   @param start [Integer] The index to start retrieving items from.
    #   @param length [Integer] The number of items to retrieve.
    # @overload set[range]
    #   Return a subset specified by the given `range` of indices.
    #   @param range [Range] The range of indices to retrieve.
    #
    # @return [Object]
    def [](arg, length = (missing_length = true))
      if missing_length
        if arg.is_a?(Range)
          from, to = arg.begin, arg.end
          from += @node.size if from < 0
          to   += @node.size if to < 0
          to   += 1     if !arg.exclude_end?
          length = to - from
          length = 0 if length < 0
          subsequence(from, length)
        else
          at(arg)
        end
      else
        arg += @node.size if arg < 0
        subsequence(arg, length)
      end
    end
    def_delegator :self, :[], :slice

    # Return a new `SortedSet` with only the elements at the given `indices`.
    # If any of the `indices` do not exist, they will be skipped.
    #
    # @param indices [Array] The indices to retrieve and gather into a new `SortedSet`
    # @return [SortedSet]
    def values_at(*indices)
      indices.select! { |i| i >= -@node.size && i < @node.size }
      self.class.new(indices.map! { |i| at(i) })
    end

    # Call the given block once for each item in the set, passing each
    # item from first to last successively to the block.
    #
    # @return [self]
    def each(&block)
      return @node.to_enum if not block_given?
      @node.each(&block)
      self
    end

    # Call the given block once for each item in the set, passing each
    # item starting from the last, and counting back to the first, successively to
    # the block.
    #
    # @return [self]
    def reverse_each(&block)
      return @node.enum_for(:reverse_each) if not block_given?
      @node.reverse_each(&block)
      self
    end

    # Return the "lowest" element in this set, as determined by its sort order.
    # @return [Object]
    def min
      @node.min
    end
    alias :first :min
    def_delegator :self, :first, :head

    # Return the "highest" element in this set, as determined by its sort order.
    # @return [Object]
    def max
      @node.max
    end
    alias :last :max

    # Return a new `SortedSet` containing all elements for which the given block returns
    # true.
    #
    # @return [SortedSet]
    def filter
      return enum_for(:filter) unless block_given?
      reduce(self) { |set, item| yield(item) ? set : set.delete(item) }
    end

    # Invoke the given block once for each item in the set, and return a new
    # `SortedSet` containing the values returned by the block.
    #
    # @return [SortedSet]
    def map
      return enum_for(:map) if not block_given?
      return self if empty?
      self.class.new(super, &@comparator)
    end
    def_delegator :self, :map, :collect

    # Return `true` if the given item is present in this `SortedSet`. More precisely,
    # return `true` if an object which compares as "equal" using this set's
    # comparator is present.
    #
    # @param item [Object] The object to check for
    # @return [Boolean]
    def include?(item)
      @node.include?(item, @comparator)
    end
    def_delegator :self, :include?, :member?

    # Return a new `SortedSet` with the same items, but a sort order determined by
    # the given block.
    #
    # @return [SortedSet]
    def sort(&block)
      block ||= lambda { |a,b| a <=> b }
      self.class.new(self.to_a, &block)
    end
    alias :sort_by :sort

    # Return the index of the first object in this set which is equal to
    # `obj`. Rather than using `#==`, we use `#<=>` (or our comparator block) for
    # comparisons. This means we can find the index in O(log N) time, rather than O(N).
    #
    # @param obj [Object] The object to search for
    # @return [Integer]
    def find_index(obj = (missing_obj = true), &block)
      if !missing_obj
        # Enumerable provides a default implementation, but this is more efficient
        node = @node
        index = node.left.size
        while !node.empty?
          direction = node.direction(obj, @comparator)
          if direction > 0
            node = node.right
            index += node.left.size
          elsif direction < 0
            node = node.left
            index -= node.right.size
          else
            return index
          end
        end
        nil
      else
        super(&block)
      end
    end
    def_delegator :self, :find_index, :index

    # Drop the first `n` elements and return the rest in a new `SortedSet`.
    # @param n [Integer] The number of elements to remove
    # @return [SortedSet]
    def drop(n)
      self.class.new(super)
    end

    # Return only the first `n` elements in a new `SortedSet`.
    # @param n [Integer] The number of elements to retain
    # @return [SortedSet]
    def take(n)
      self.class.new(super)
    end

    # Drop elements up to, but not including, the first element for which the
    # block returns `nil` or `false`. Gather the remaining elements into a new
    # `SortedSet`. If no block is given, an `Enumerator` is returned instead.
    #
    # @return [SortedSet, Enumerator]
    def drop_while
      return enum_for(:drop_while) if not block_given?
      self.class.new(super)
    end

    # Gather elements up to, but not including, the first element for which the
    # block returns `nil` or `false`, and return them in a new `SortedSet`. If no block
    # is given, an `Enumerator` is returned instead.
    #
    # @return [SortedSet, Enumerator]
    def take_while
      return enum_for(:take_while) if not block_given?
      self.class.new(super)
    end

    # Return a new `SortedSet` which contains all the members of both this set and `other`.
    # `other` can be any `Enumerable` object.
    #
    # @example
    #   Hamster::SortedSet[1, 2] | Hamster::SortedSet[2, 3]
    #   # => Hamster::SortedSet[1, 2, 3]
    #
    # @param other [Enumerable] The collection to merge with
    # @return [SortedSet]
    def union(other)
      self.class.alloc(@node.bulk_insert(other, @comparator), @comparator)
    end
    def_delegator :self, :union, :|
    def_delegator :self, :union, :+
    def_delegator :self, :union, :merge

    # Return a new `SortedSet` which contains all the items which are members of both
    # this set and `other`. `other` can be any `Enumerable` object.
    #
    # @example
    #   Hamster::SortedSet[1, 2] & Hamster::SortedSet[2, 3]
    #   # => Hamster::SortedSet[2]
    #
    # @param other [Enumerable] The collection to intersect with
    # @return [SortedSet]
    def intersection(other)
      self.class.alloc(@node.keep_only(other, @comparator), @comparator)
    end
    def_delegator :self, :intersection, :intersect
    def_delegator :self, :intersection, :&

    # Return a new `SortedSet` with all the items in `other` removed. `other` can be
    # any `Enumerable` object.
    #
    # @example
    #   Hamster::SortedSet[1, 2] - Hamster::SortedSet[2, 3]
    #   # => Hamster::SortedSet[1]
    #
    # @param other [Enumerable] The collection to subtract from this set
    # @return [SortedSet]
    def difference(other)
      self.class.alloc(@node.bulk_delete(other, @comparator), @comparator)
    end
    def_delegator :self, :difference, :diff
    def_delegator :self, :difference, :subtract
    def_delegator :self, :difference, :-

    # Return a new `SortedSet` with all the items which are members of this
    # set or of `other`, but not both. `other` can be any `Enumerable` object.
    #
    # @example
    #   Hamster::SortedSet[1, 2] ^ Hamster::SortedSet[2, 3]
    #   # => Hamster::SortedSet[1, 3]
    #
    # @param other [Enumerable] The collection to take the exclusive disjunction of
    # @return [SortedSet]
    def exclusion(other)
      ((self | other) - (self & other))
    end
    def_delegator :self, :exclusion, :^

    # Return `true` if all items in this set are also in `other`.
    #
    # @param other [Enumerable]
    # @return [Boolean]
    def subset?(other)
      return false if other.size < size
      all? { |item| other.include?(item) }
    end

    # Return `true` if all items in `other` are also in this set.
    #
    # @param other [Enumerable]
    # @return [Boolean]
    def superset?(other)
      other.subset?(self)
    end

    # Returns `true` if `other` contains all the items in this set, plus at least
    # one item which is not in this set.
    #
    # @param other [Enumerable]
    # @return [Boolean]
    def proper_subset?(other)
      return false if other.size <= size
      all? { |item| other.include?(item) }
    end

    # Returns `true` if this set contains all the items in `other`, plus at least
    # one item which is not in `other`.
    #
    # @param other [Enumerable]
    # @return [Boolean]
    def proper_superset?(other)
      other.proper_subset?(self)
    end

    # Return `true` if this set and `other` do not share any items.
    #
    # @param other [Enumerable]
    # @return [Boolean]
    def disjoint?(other)
      if size < other.size
        each { |item| return false if other.include?(item) }
      else
        other.each { |item| return false if include?(item) }
      end
      true
    end

    # Return `true` if this set and `other` have at least one item in common.
    #
    # @param other [Enumerable]
    # @return [Boolean]
    def intersect?(other)
      !disjoint?(other)
    end

    def_delegator :self, :group_by, :group
    def_delegator :self, :group_by, :classify

    # With a block, yield all the items which are "higher" than `item` (as defined
    # by the set's comparator). Otherwise, return them as a new `SortedSet`.
    #
    # @param item [Object]
    def above(item, &block)
      if block_given?
        @node.each_greater(item, @comparator, false, &block)
      else
        self.class.alloc(@node.suffix(item, @comparator, false), @comparator)
      end
    end

    # With a block, yield all the items which are "higher" than `item` (as defined
    # by the set's comparator). Otherwise, return them as a new `SortedSet`.
    #
    # @param item [Object]
    def below(item, &block)
      if block_given?
        @node.each_less(item, @comparator, false, &block)
      else
        self.class.alloc(@node.prefix(item, @comparator, false), @comparator)
      end
    end

    # With a block, yield all the items which are "higher" or equal to `item`
    # (as determined by the set's comparator). Otherwise, return them as a new
    # `SortedSet`.
    #
    # @param item [Object]
    def from(item, &block)
      if block_given?
        @node.each_greater(item, @comparator, true, &block)
      else
        self.class.alloc(@node.suffix(item, @comparator, true), @comparator)
      end
    end

    # With a block, yield all the items which are "lower" than `item` (as defined
    # by the set's comparator). Otherwise, return them as a new `SortedSet`.
    #
    # @param item [Object]
    def up_to(item, &block)
      if block_given?
        @node.each_less(item, @comparator, true, &block)
      else
        self.class.alloc(@node.prefix(item, @comparator, true), @comparator)
      end
    end

    # With a block, yield all the items which are equal or higher than `from` and
    # equal or less than `to` (as determined by the set's comparator). Otherwise,
    # return the specified range of items as a new `SortedSet`.
    #
    # @param from [Object]
    # @param to [Object]
    def between(from, to, &block)
      if block_given?
        @node.each_between(from, to, @comparator, &block)
      else
        self.class.alloc(@node.between(from, to, @comparator), @comparator)
      end
    end

    # Return a randomly chosen item from this set. If the set is empty, return `nil`.
    #
    # @return [Object]
    def sample
      @node.at(rand(@node.size))
    end

    # Return an empty `SortedSet` instance, of the same class as this one. Useful if you
    # have multiple subclasses of `SortedSet` and want to treat them polymorphically.
    #
    # @return [Hash]
    def clear
      self.class.empty
    end

    # Return true if `other` has the same type and contents as this `SortedSet`.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean]
    def eql?(other)
      return true if other.equal?(self)
      return false if not instance_of?(other.class)
      return false if size != other.size
      a, b = self.to_enum, other.to_enum
      while true
        return false if !a.next.eql?(b.next)
      end
    rescue StopIteration
      true
    end

    # See `Object#hash`.
    # @return [Integer]
    def hash
      reduce(0) { |hash, item| (hash << 5) - hash + item.hash }
    end

    def_delegator :self, :dup, :uniq
    def_delegator :self, :dup, :nub
    def_delegator :self, :dup, :remove_duplicates

    # @return [::Array]
    # @private
    def marshal_dump
      if @comparator
        raise TypeError, "can't dump SortedSet with custom sort order"
      else
        to_a
      end
    end

    # @private
    def marshal_load(array)
      initialize(array)
    end

    private

    def subsequence(from, length)
      return nil if from > @node.size || from < 0 || length < 0
      length = @node.size - from if @node.size < from + length
      return self.class.empty if length == 0
      self.class.alloc(@node.slice(from, length), @comparator)
    end

    # @private
    class AVLNode
      def self.from_items(items, from, to) # items must be sorted
        size = to - from + 1
        if size >= 3
          middle = (to + from) / 2
          AVLNode.new(items[middle], AVLNode.from_items(items, from, middle-1), AVLNode.from_items(items, middle+1, to))
        elsif size == 2
          AVLNode.new(items[from], EmptyAVLNode, AVLNode.new(items[from+1], EmptyAVLNode, EmptyAVLNode))
        elsif size == 1
          AVLNode.new(items[from], EmptyAVLNode, EmptyAVLNode)
        elsif size == 0
          EmptyAVLNode
        end
      end

      def initialize(item, left, right)
        @item, @left, @right = item, left, right
        @height = ((@right.height > @left.height) ? @right.height : @left.height) + 1
        @size   = @right.size + @left.size + 1
      end
      attr_reader :item, :left, :right, :height, :size

      def empty?
        false
      end

      def insert(item, comparator)
        dir = direction(item, comparator)
        if dir == 0
          self
        elsif dir > 0
          rebalance_right(@left, @right.insert(item, comparator))
        else
          rebalance_left(@left.insert(item, comparator), @right)
        end
      end

      def bulk_insert(items, comparator)
        return self if items.empty?
        return insert(items.first, comparator) if items.size == 1

        left, right = partition(items, comparator)

        if right.size > left.size
          rebalance_right(@left.bulk_insert(left, comparator), @right.bulk_insert(right, comparator))
        else
          rebalance_left(@left.bulk_insert(left, comparator), @right.bulk_insert(right, comparator))
        end
      end

      def delete(item, comparator)
        dir = direction(item, comparator)
        if dir == 0
          if @right.empty?
            return @left # replace this node with its only child
          elsif @left.empty?
            return @right # likewise
          end

          if balance > 0
            # tree is leaning to the left. replace with highest node on that side
            replace_with = @left.max
            AVLNode.new(replace_with, @left.delete(replace_with, comparator), @right)
          else
            # tree is leaning to the right. replace with lowest node on that side
            replace_with = @right.min
            AVLNode.new(replace_with, @left, @right.delete(replace_with, comparator))
          end
        elsif dir > 0
          rebalance_left(@left, @right.delete(item, comparator))
        else
          rebalance_right(@left.delete(item, comparator), @right)
        end
      end

      def bulk_delete(items, comparator)
        return self if items.empty?
        return delete(items.first, comparator) if items.size == 1

        left, right, keep_item = [], [], true
        items.each do |item|
          dir = direction(item, comparator)
          if dir > 0
            right << item
          elsif dir < 0
            left << item
          else
            keep_item = false
          end
        end

        left  = @left.bulk_delete(left, comparator)
        right = @right.bulk_delete(right, comparator)
        finish_removal(keep_item, left, right, comparator)
      end

      def keep_only(items, comparator)
        return EmptyAVLNode if items.empty?

        left, right, keep_item = [], [], false
        items.each do |item|
          dir = direction(item, comparator)
          if dir > 0
            right << item
          elsif dir < 0
            left << item
          else
            keep_item = true
          end
        end

        left  = @left.keep_only(left, comparator)
        right = @right.keep_only(right, comparator)
        finish_removal(keep_item, left, right, comparator)
      end

      def finish_removal(keep_item, left, right, comparator)
        # deletion of items may have occurred on left and right sides
        # now we may also need to delete the current item
        if keep_item
          rebalance(left, right) # no need to delete the current item
        elsif left.empty?
          right
        elsif right.empty?
          left
        elsif left.height > right.height
          replace_with = left.max
          AVLNode.new(replace_with, left.delete(replace_with, comparator), right)
        else
          replace_with = right.min
          AVLNode.new(replace_with, left, right.delete(replace_with, comparator))
        end
      end

      def prefix(item, comparator, inclusive)
        dir = direction(item, comparator)
        if dir > 0 || (inclusive && dir == 0)
          rebalance_left(@left, @right.prefix(item, comparator, inclusive))
        else
          @left.prefix(item, comparator, inclusive)
        end
      end

      def suffix(item, comparator, inclusive)
        dir = direction(item, comparator)
        if dir < 0 || (inclusive && dir == 0)
          rebalance_right(@left.suffix(item, comparator, inclusive), @right)
        else
          @right.suffix(item, comparator, inclusive)
        end
      end

      def between(from, to, comparator)
        if direction(from, comparator) > 0 # all on the right
          @right.between(from, to, comparator)
        elsif direction(to, comparator) < 0 # all on the left
          @left.between(from, to, comparator)
        else
          left = @left.suffix(from, comparator, true)
          right = @right.prefix(to, comparator, true)
          rebalance(left, right)
        end
      end

      def each_less(item, comparator, inclusive, &block)
        dir = direction(item, comparator)
        if dir > 0 || (inclusive && dir == 0)
          @left.each(&block)
          yield @item
          @right.each_less(item, comparator, inclusive, &block)
        else
          @left.each_less(item, comparator, inclusive, &block)
        end
      end

      def each_greater(item, comparator, inclusive, &block)
        dir = direction(item, comparator)
        if dir < 0 || (inclusive && dir == 0)
          @left.each_greater(item, comparator, inclusive, &block)
          yield @item
          @right.each(&block)
        else
          @right.each_greater(item, comparator, inclusive, &block)
        end
      end

      def each_between(from, to, comparator, &block)
        if direction(from, comparator) > 0 # all on the right
          @right.each_between(from, to, comparator, &block)
        elsif direction(to, comparator) < 0 # all on the left
          @left.each_between(from, to, comparator, &block)
        else
          @left.each_greater(from, comparator, true, &block)
          yield @item
          @right.each_less(to, comparator, true, &block)
        end
      end

      def each(&block)
        @left.each(&block)
        yield @item
        @right.each(&block)
      end

      def reverse_each(&block)
        @right.reverse_each(&block)
        yield @item
        @left.reverse_each(&block)
      end

      def include?(item, comparator)
        dir = direction(item, comparator)
        if dir == 0
          true
        elsif dir > 0
          @right.include?(item, comparator)
        else
          @left.include?(item, comparator)
        end
      end

      def at(index)
        if index < @left.size
          @left.at(index)
        elsif index > @left.size
          @right.at(index - @left.size - 1)
        else
          @item
        end
      end

      def max
        @right.empty? ? @item : @right.max
      end

      def min
        @left.empty? ? @item : @left.min
      end

      def balance
        @left.height - @right.height
      end

      def slice(from, length)
        if length <= 0
          EmptyAVLNode
        elsif from + length <= @left.size
          @left.slice(from, length)
        elsif from > @left.size
          @right.slice(from - @left.size - 1, length)
        else
          left  = @left.slice(from, @left.size - from)
          right = @right.slice(0, from + length - @left.size - 1)
          rebalance(left, right)
        end
      end

      def partition(items, comparator)
        left, right = [], []
        items.each do |item|
          dir = direction(item, comparator)
          if dir > 0
            right << item
          elsif dir < 0
            left << item
          end
        end
        [left, right]
      end

      def rebalance(left, right)
        if left.height > right.height
          rebalance_left(left, right)
        else
          rebalance_right(left, right)
        end
      end

      def rebalance_left(left, right)
        # the tree might be unbalanced to the left (paths on the left too long)
        balance = left.height - right.height
        if balance >= 2
          if left.balance > 0
            # single right rotation
            AVLNode.new(left.item, left.left, AVLNode.new(@item, left.right, right))
          else
            # left rotation, then right
            AVLNode.new(left.right.item, AVLNode.new(left.item, left.left, left.right.left), AVLNode.new(@item, left.right.right, right))
          end
        else
          AVLNode.new(@item, left, right)
        end
      end

      def rebalance_right(left, right)
        # the tree might be unbalanced to the right (paths on the right too long)
        balance = left.height - right.height
        if balance <= -2
          if right.balance > 0
            # right rotation, then left
            AVLNode.new(right.left.item, AVLNode.new(@item, left, right.left.left), AVLNode.new(right.item, right.left.right, right.right))
          else
            # single left rotation
            AVLNode.new(right.item, AVLNode.new(@item, left, right.left), right.right)
          end
        else
          AVLNode.new(@item, left, right)
        end
      end

      def direction(item, comparator)
        if comparator
          comparator.call(item, @item)
        else
          item <=> @item
        end
      end
    end

    EmptyAVLNode = Object.new.tap do |e|
      def e.left;  self; end
      def e.right; self; end
      def e.height;   0; end
      def e.size;     0; end
      def e.min;    nil; end
      def e.max;    nil; end
      def e.each;        end
      def e.reverse_each; end
      def e.at(index); nil; end
      def e.insert(item, comparator); AVLNode.new(item, self, self); end
      def e.bulk_insert(items, comparator)
        items = items.to_a if !items.is_a?(Array)
        AVLNode.from_items(items.sort(&comparator), 0, items.size-1)
      end
      def e.bulk_delete(items, comparator); self; end
      def e.keep_only(items, comparator); self; end
      def e.delete(item, comparator); self; end
      def e.include?(item, comparator); false; end
      def e.prefix(item, comparator, inclusive); self; end
      def e.suffix(item, comparator, inclusive); self; end
      def e.between(from, to, comparator); self; end
      def e.each_greater(item, comparator, inclusive); end
      def e.each_less(item, comparator, inclusive); end
      def e.each_between(item, comparator, inclusive); end
      def e.empty?; true; end
      def e.slice(from, length); self; end
    end.freeze
  end

  # The canonical empty `SortedSet`. Returned by `Hamster.sorted_set` and `SortedSet[]`
  # when invoked with no arguments; also returned by `SortedSet.empty`. Prefer using
  # this one rather than creating many empty sorted sets using `SortedSet.new`.
  #
  EmptySortedSet = Hamster::SortedSet.empty
end