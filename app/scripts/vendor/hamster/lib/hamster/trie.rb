require "forwardable"

module Hamster
  class Trie
    extend Forwardable

    # Returns the number of key-value pairs in the trie.
    attr_reader :size

    def initialize(significant_bits, size = 0, entries = [], children = [])
      @significant_bits = significant_bits
      @entries = entries
      @children = children
      @size = size
    end

    # Returns <tt>true</tt> if the trie contains no key-value pairs.
    def empty?
      size == 0
    end

    # Returns <tt>true</tt> if the given key is present in the trie.
    def key?(key)
      !!get(key)
    end
    def_delegator :self, :key?, :has_key?

    # Calls <tt>block</tt> once for each entry in the trie, passing the key-value pair as parameters.
    def each
      @entries.each { |entry| yield(entry) if entry }
      @children.each do |child|
        child.each { |entry| yield(entry) } if child
      end
      nil
    end

    def reduce(memo)
      each { |entry| memo = yield(memo, entry) }
      memo
    end

    def filter
      reduce(self) { |trie, entry| yield(entry) ? trie : trie.delete(entry.key) }
    end

    # Returns a copy of <tt>self</tt> with the given value associated with the key.
    def put(key, value)
      index = index_for(key)
      entry = @entries[index]

      if !entry
        entries = @entries.dup
        entries[index] = Entry.new(key, value)
        self.class.new(@significant_bits, @size + 1, entries, @children)
      elsif entry.key.eql?(key)
        entries = @entries.dup
        entries[index] = Entry.new(key, value)
        self.class.new(@significant_bits, @size, entries, @children)
      else
        children = @children.dup
        child = children[index]
        child_size = child ? child.size : 0
        if child
          children[index] = child.put(key, value)
        else
          children[index] = self.class.new(@significant_bits + 5).put!(key, value)
        end
        new_child_size = children[index].size
        new_self_size = @size + (new_child_size - child_size)
        self.class.new(@significant_bits, new_self_size, @entries, children)
      end
    end

    # Retrieves the entry corresponding to the given key. If not found, returns <tt>nil</tt>.
    def get(key)
      index = index_for(key)
      entry = @entries[index]
      if entry && entry.key.eql?(key)
        entry
      else
        child = @children[index]
        child.get(key) if child
      end
    end

    # Returns a copy of <tt>self</tt> with the given key (and associated value) deleted. If not found, returns <tt>self</tt>.
    def delete(key)
      find_and_delete(key) || self.class.new(@significant_bits)
    end

    def include?(key, value)
      entry = get(key)
      entry && value.eql?(entry.value)
    end

    # Returns <tt>true</tt> if . <tt>eql?</tt> is synonymous with <tt>==</tt>
    def eql?(other)
      return true if equal?(other)
      return false unless instance_of?(other.class) && size == other.size
      each do |entry|
        return false unless other.include?(entry.key, entry.value)
      end
      true
    end
    def_delegator :self, :eql?, :==

    protected

    # Returns <tt>self</tt> after overwriting the element associated with the specified key.
    def put!(key, value)
      index = index_for(key)
      @size += 1 unless @entries[index]
      @entries[index] = Entry.new(key, value)
      self
    end

    # Returns a replacement instance after removing the specified key.
    # If not found, returns <tt>self</tt>.
    # If empty, returns <tt>nil</tt>.
    def find_and_delete(key)
      index = index_for(key)
      entry = @entries[index]
      if entry && entry.key.eql?(key)
        return delete_at(index)
      else
        child = @children[index]
        if child
          copy = child.find_and_delete(key)
          unless copy.equal?(child)
            children = @children.dup
            children[index] = copy
            new_size = @size - (child.size - copy_size(copy))
            return self.class.new(@significant_bits, new_size, @entries, children)
          end
        end
      end
      self
    end

    # Returns a replacement instance after removing the specified entry. If empty, returns <tt>nil</tt>
    def delete_at(index = @entries.index { |e| e })
      yield(@entries[index]) if block_given?
      if size > 1
        entries = @entries.dup
        child = @children[index]
        if child
          children = @children.dup
          children[index] = child.delete_at do |entry|
            entries[index] = entry
          end
        else
          entries[index] = nil
        end
        self.class.new(@significant_bits, @size - 1, entries, children || @children)
      end
    end

    private

    def index_for(key)
      (key.hash.abs >> @significant_bits) & 31
    end

    def copy_size(copy)
      copy ? copy.size : 0
    end

    class Entry
      attr_reader :key, :value

      def initialize(key, value)
        @key = key
        @value = value
      end
    end
  end

  EmptyTrie = Hamster::Trie.new(0)
end
