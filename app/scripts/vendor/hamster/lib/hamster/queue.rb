require "forwardable"
require "hamster/immutable"
require "hamster/list"

module Hamster
  def self.queue(*items)
    items.reduce(EmptyQueue) { |queue, item| queue.enqueue(item) }
  end

  class Queue
    extend Forwardable
    include Immutable

    def initialize
      @front = @rear = EmptyList
    end

    def empty?
      @front.empty? && @rear.empty?
    end
    def_delegator :self, :empty?, :null?

    def size
      @front.size + @rear.size
    end
    def_delegator :self, :size, :length

    def head
      return @front.head unless @front.empty?
      @rear.last
    end
    def_delegator :self, :head, :first
    def_delegator :self, :head, :peek
    def_delegator :self, :head, :front

    def enqueue(item)
      transform { @rear = @rear.cons(item) }
    end
    def_delegator :self, :enqueue, :<<
    def_delegator :self, :enqueue, :add

    def dequeue
      front = @front
      rear = @rear
      if front.empty?
        return EmptyQueue if rear.empty?
        front = rear.reverse
        rear = EmptyList
      end

      transform do
        @front = front.tail
        @rear = rear
      end
    end
    def_delegator :self, :dequeue, :tail

    def clear
      EmptyQueue
    end

    def eql?(other)
      instance_of?(other.class) &&
        to_list.eql?(other.to_list)
    end
    def_delegator :self, :eql?, :==

    def to_a
      to_list.to_a
    end
    def_delegator :self, :to_a, :entries

    def to_ary
      to_list.to_ary
    end

    def to_list
      @front.append(@rear.reverse)
    end

    def inspect
      to_list.inspect
    end
  end

  EmptyQueue = Hamster::Queue.new
end
