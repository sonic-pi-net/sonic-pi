require "forwardable"
require "hamster/immutable"

module Hamster
  class Tuple
    extend Forwardable
    include Immutable

    def initialize(*items)
      @items = items.freeze
    end

    def first
      @items.first
    end

    def last
      @items.last
    end

    def eql?(other)
      return true if other.equal?(self)
      instance_of?(other.class) && @items.eql?(other.instance_variable_get(:@items))
    end
    def_delegator :self, :eql?, :==

    def to_ary
      @items
    end

    def to_a
      @items.dup
    end

    def inspect
      "(#{@items.inspect[1..-2]})"
    end
  end
end
