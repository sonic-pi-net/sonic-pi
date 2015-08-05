module Concurrent
  module Synchronization
    class Condition < Object

      singleton_class.send :alias_method, :private_new, :new
      private_class_method :new

      def initialize(lock)
        @Lock = lock
        ensure_ivar_visibility!
        super()
      end

      def wait(timeout = nil)
        @Lock.synchronize { ns_wait(timeout) }
      end

      def ns_wait(timeout = nil)
        synchronize { super(timeout) }
      end

      def wait_until(timeout = nil, &condition)
        @Lock.synchronize { ns_wait_until(timeout, &condition) }
      end

      def ns_wait_until(timeout = nil, &condition)
        synchronize { super(timeout, &condition) }
      end

      def signal
        @Lock.synchronize { ns_signal }
      end

      def ns_signal
        synchronize { super }
      end

      def broadcast
        @Lock.synchronize { ns_broadcast }
      end

      def ns_broadcast
        synchronize { super }
      end
    end

    class Object < Implementation
      def new_condition
        Condition.private_new(self)
      end
    end
  end
end
