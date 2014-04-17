require "forwardable"
require "hamster/undefined"
require "hamster/tuple"

module Hamster
  module Enumerable
    extend Forwardable

    def each
      fail NoMethodError, "undefined method `each' for #{self.class.name}"
    end
    def_delegator :self, :each, :foreach

    def filter
      fail NoMethodError, "undefined method `filter' for #{self.class.name}"
    end
    def_delegator :self, :filter, :select
    def_delegator :self, :filter, :find_all

    def each_with_index(&block)
      return self unless block_given?
      reduce(0) do |index, item|
        yield(item, index)
        index + 1
      end
      nil
    end

    def reduce(memo = Undefined)
      each do |item|
        memo = memo.equal?(Undefined) ? item : yield(memo, item)
      end if block_given?
      Undefined.erase(memo)
    end
    def_delegator :self, :reduce, :inject
    def_delegator :self, :reduce, :fold
    def_delegator :self, :reduce, :foldr

    def partition(&block)
      return self unless block_given?
      Tuple.new(filter(&block), remove(&block))
    end

    def find
      return nil unless block_given?
      each { |item| return item if yield(item) }
    end
    def_delegator :self, :find, :detect

    def include?(object)
      any? { |item| item == object }
    end
    def_delegator :self, :include?, :member?
    def_delegator :self, :include?, :contains?
    def_delegator :self, :include?, :elem?

    def any?
      return any? { |item| item } unless block_given?
      each { |item| return true if yield(item) }
      false
    end
    def_delegator :self, :any?, :exist?
    def_delegator :self, :any?, :exists?

    def all?
      return all? { |item| item } unless block_given?
      each { |item| return false unless yield(item) }
      true
    end
    def_delegator :self, :all?, :forall?

    def none?
      return none? { |item| item } unless block_given?
      each { |item| return false if yield(item) }
      true
    end

    def one?
      return one? { |item| !!item } unless block_given?
      reduce(false) do |previously_matched, item|
        if yield(item)
          return false if previously_matched
          true
        else
          previously_matched
        end
      end
    end

    def minimum(&block)
      return minimum { |minimum, item| item <=> minimum } unless block_given?
      reduce { |minimum, item| yield(minimum, item) < 0 ? item : minimum }
    end
    def_delegator :self, :minimum, :min

    def maximum(&block)
      return maximum { |maximum, item| item <=> maximum } unless block_given?
      reduce { |maximum, item| yield(maximum, item) > 0 ? item : maximum }
    end
    def_delegator :self, :maximum, :max

    def grep(pattern, &block)
      filter { |item| pattern === item  }.map(&block)
    end

    def count(&block)
      return size unless block_given?
      reduce(0) { |count, item| yield(item) ? count + 1 : count }
    end

    def product
      reduce(1, &:*)
    end

    def sum
      reduce(0, &:+)
    end

    def remove
      return self unless block_given?
      filter { |item| !yield(item) }
    end
    def_delegator :self, :remove, :reject
    def_delegator :self, :remove, :delete_if

    def compact
      remove(&:nil?)
    end

    def to_a
      reduce([]) { |a, item| a << item }
    end
    def_delegator :self, :to_a, :entries
    def_delegator :self, :to_a, :to_ary
  end
end
