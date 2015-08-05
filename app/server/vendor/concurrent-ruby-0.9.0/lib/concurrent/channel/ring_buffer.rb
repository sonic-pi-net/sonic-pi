module Concurrent
  module Channel

    # non-thread safe buffer
    #
    # @api Channel
    # @!macro edge_warning
    class RingBuffer

      def initialize(capacity)
        @buffer = Array.new(capacity)
        @first = @last = 0
        @count = 0
      end


      # @return [Integer] the capacity of the buffer
      def capacity
        @buffer.size
      end

      # @return [Integer] the number of elements currently in the buffer
      def count
        @count
      end

      # @return [Boolean] true if buffer is empty, false otherwise
      def empty?
        @count == 0
      end

      # @return [Boolean] true if buffer is full, false otherwise
      def full?
        @count == capacity
      end

      # @param [Object] value
      # @return [Boolean] true if value has been inserted, false otherwise
      def offer(value)
        return false if full?

        @buffer[@last] = value
        @last = (@last + 1) % @buffer.size
        @count += 1
        true
      end

      # @return [Object] the first available value and removes it from the buffer. If buffer is empty returns nil
      def poll
        result = @buffer[@first]
        @buffer[@first] = nil
        @first = (@first + 1) % @buffer.size
        @count -= 1
        result
      end

      # @return [Object] the first available value and without removing it from
      #   the buffer. If buffer is empty returns nil
      def peek
        @buffer[@first]
      end

    end
  end
end
