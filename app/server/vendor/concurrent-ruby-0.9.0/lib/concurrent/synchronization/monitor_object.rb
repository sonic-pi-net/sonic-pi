require 'monitor'

module Concurrent
  module Synchronization

    # @!visibility private
    # @!macro internal_implementation_note
    class MonitorObject < MutexObject
      def initialize
        @__lock__      = ::Monitor.new
        @__condition__ = @__lock__.new_cond
      end

      protected

      def synchronize
        @__lock__.synchronize { yield }
      end

      def ns_wait(timeout = nil)
        @__condition__.wait timeout
        self
      end
    end
  end
end
