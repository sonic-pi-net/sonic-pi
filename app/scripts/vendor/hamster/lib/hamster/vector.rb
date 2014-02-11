require "forwardable"
require "hamster/undefined"
require "hamster/immutable"
require "hamster/enumerable"

module Hamster
  def self.vector(*items)
    items.reduce(EmptyVector) { |vector, item| vector.add(item) }
  end

  class Vector
    extend Forwardable
    include Immutable
    include Enumerable

    BLOCK_SIZE = 32
    INDEX_MASK = BLOCK_SIZE - 1
    BITS_PER_LEVEL = 5

    attr_reader :size
    def_delegator :self, :size, :length

    def initialize
      @levels = 0
      @root = []
      @size = 0
    end

    def empty?
      @size == 0
    end
    def_delegator :self, :empty?, :null?

    def first
      get(0)
    end
    def_delegator :self, :first, :head

    def last
      get(-1)
    end

    def add(item)
      transform do
        update_leaf_node(@size, item)
        @size += 1
      end
    end
    def_delegator :self, :add, :<<
    def_delegator :self, :add, :cons

    # def delete(index)
    # end

    def set(index, item = Undefined)
      return set(index, yield(get(index))) if item.equal?(Undefined)
      raise IndexError if empty? || index == @size
      raise IndexError if index.abs > @size
      return set(@size + index, item) if index < 0
      transform do
        update_leaf_node(index, item)
      end
    end

    def get(index)
      return nil if empty? || index == @size
      return nil if index.abs > @size
      return get(@size + index) if index < 0
      leaf_node_for(@root, root_index_bits, index)[index & INDEX_MASK]
    end
    def_delegator :self, :get, :[]
    def_delegator :self, :get, :at

    def each(&block)
      return self unless block_given?
      traverse_depth_first(&block)
      nil
    end

    def map(&block)
      return self unless block_given?
      reduce(EmptyVector) { |vector, item| vector.add(yield(item)) }
    end
    def_delegator :self, :map, :collect

    def filter
      return self unless block_given?
      reduce(EmptyVector) { |vector, item| yield(item) ? vector.add(item) : vector }
    end

    def clear
      EmptyVector
    end

    def inspect
      to_a.inspect
    end

    def eql?(other)
      return true if other.equal?(self)
      return false unless instance_of?(other.class) && @size == other.size
      @root.eql?(other.instance_variable_get(:@root))
    end
    def_delegator :self, :eql?, :==

    private

    def traverse_depth_first(node = @root, level = @levels, &block)
      return node.each(&block) if level == 0
      node.each { |child| traverse_depth_first(child, level - 1, &block) }
    end

    def leaf_node_for(node, child_index_bits, index)
      return node if child_index_bits == 0
      child_index = (index >> child_index_bits) & INDEX_MASK
      leaf_node_for(node[child_index], child_index_bits - BITS_PER_LEVEL, index)
    end

    def update_leaf_node(index, item)
      copy_leaf_node_for(new_root, root_index_bits, index)[index & INDEX_MASK] = item
    end

    def copy_leaf_node_for(node, child_index_bits, index)
      return node if child_index_bits == 0
      child_index = (index >> child_index_bits) & INDEX_MASK
      child_node = node[child_index]
      if child_node
        child_node = child_node.dup
      else
        child_node = []
      end
      node[child_index] = child_node
      copy_leaf_node_for(child_node, child_index_bits - BITS_PER_LEVEL, index)
    end

    def new_root
      if full?
        @levels += 1
        @root = [@root]
      else
        @root = @root.dup
      end
    end

    def full?
      (@size >> root_index_bits) > 0
    end

    def root_index_bits
      @levels * BITS_PER_LEVEL
    end
  end

  EmptyVector = Hamster::Vector.new
end
