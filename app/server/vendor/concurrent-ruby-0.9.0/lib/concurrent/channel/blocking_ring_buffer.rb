require 'concurrent/synchronization'

module Concurrent
  module Channel

    # @api Channel
    # @!macro edge_warning
    class BlockingRingBuffer < Synchronization::Object

      def initialize(capacity)
        super()
        synchronize { ns_initialize capacity}
      end

      # @return [Integer] the capacity of the buffer
      def capacity
        synchronize { @buffer.capacity }
      end

      # @return [Integer] the number of elements currently in the buffer
      def count
        synchronize { @buffer.count }
      end

      # @return [Boolean] true if buffer is empty, false otherwise
      def empty?
        synchronize { @buffer.empty? }
      end

      # @return [Boolean] true if buffer is full, false otherwise
      def full?
        synchronize { @buffer.full? }
      end

      # @param [Object] value the value to be inserted
      # @return [Boolean] true if value has been inserted, false otherwise
      def put(value)
        synchronize do
          wait_while_full
          @buffer.offer(value)
          ns_signal
          true
        end
      end

      # @return [Object] the first available value and removes it from the buffer.
      #   If buffer is empty it blocks until an element is available
      def take
        synchronize do
          wait_while_empty
          result = @buffer.poll
          ns_signal
          result
        end
      end

      # @return [Object] the first available value and without removing it from
      #   the buffer. If buffer is empty returns nil
      def peek
        synchronize { @buffer.peek }
      end

      protected

      def ns_initialize(capacity)
        @buffer = RingBuffer.new(capacity)
        @first = @last = 0
        @count = 0
      end

      private

      def wait_while_full
        ns_wait_until { !@buffer.full? }
      end

      def wait_while_empty
        ns_wait_until { !@buffer.empty? }
      end
    end
  end
end
