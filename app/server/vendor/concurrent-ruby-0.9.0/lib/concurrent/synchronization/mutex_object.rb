module Concurrent
  module Synchronization

    # @!visibility private
    # @!macro internal_implementation_note
    class MutexObject < AbstractObject
      def initialize
        @__lock__      = ::Mutex.new
        @__condition__ = ::ConditionVariable.new
      end

      protected

      def synchronize
        if @__lock__.owned?
          yield
        else
          @__lock__.synchronize { yield }
        end
      end

      def ns_signal
        @__condition__.signal
        self
      end

      def ns_broadcast
        @__condition__.broadcast
        self
      end

      def ns_wait(timeout = nil)
        @__condition__.wait @__lock__, timeout
        self
      end

      def ensure_ivar_visibility!
        # relying on undocumented behavior of CRuby, GVL acquire has lock which ensures visibility of ivars
        # https://github.com/ruby/ruby/blob/ruby_2_2/thread_pthread.c#L204-L211
      end
    end
  end
end
