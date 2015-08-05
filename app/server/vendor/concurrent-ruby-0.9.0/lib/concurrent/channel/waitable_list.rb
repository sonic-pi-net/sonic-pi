require 'concurrent/synchronization'

module Concurrent
  module Channel

    # @api Channel
    # @!macro edge_warning
    class WaitableList < Synchronization::Object

      def initialize
        super()
        synchronize { ns_initialize }
      end

      def size
        synchronize { @list.size }
      end

      def empty?
        synchronize { @list.empty? }
      end

      def put(value)
        synchronize do
          @list << value
          ns_signal
        end
      end

      def delete(value)
        synchronize { @list.delete(value) }
      end

      def take
        synchronize do
          ns_wait_until { !@list.empty? }
          @list.shift
        end
      end

      protected

      def ns_initialize
        @list = []
      end
    end
  end
end
