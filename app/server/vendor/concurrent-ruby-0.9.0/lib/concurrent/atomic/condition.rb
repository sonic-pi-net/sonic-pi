require 'concurrent/utility/monotonic_time'
require 'concurrent/concern/deprecation'

module Concurrent

  # Condition is a better implementation of standard Ruby ConditionVariable. The
  # biggest difference is the wait return value: Condition#wait returns
  # Condition::Result which make possible to know if waiting thread has been
  # woken up by an another thread (using #signal or #broadcast) or due to
  # timeout.
  #
  # Every #wait must be guarded by a locked Mutex or a ThreadError will be
  # risen. Although it's not mandatory, it's recommended to call also #signal
  # and #broadcast within the same mutex
  #
  # @deprecated
  class Condition
    include Concern::Deprecation

    class Result
      def initialize(remaining_time)
        @remaining_time = remaining_time
      end

      attr_reader :remaining_time

      # @return [Boolean] true if current thread has been waken up by a #signal
      #  or a #broadcast call , otherwise false
      def woken_up?
        @remaining_time.nil? || @remaining_time > 0
      end

      # @return [Boolean] true if current thread has been waken up due to a
      #   timeout, otherwise false
      def timed_out?
        @remaining_time != nil && @remaining_time <= 0
      end

      alias_method :can_wait?, :woken_up?

    end

    def initialize
      deprecated 'Will be replaced with Synchronization::Object in v1.0.'
      @condition = ConditionVariable.new
    end

    # @param [Mutex] mutex the locked mutex guarding the wait
    # @param [Object] timeout nil means no timeout
    # @return [Result]
    #
    # @!macro monotonic_clock_warning
    def wait(mutex, timeout = nil)
      start_time = Concurrent.monotonic_time
      @condition.wait(mutex, timeout)

      if timeout.nil?
        Result.new(nil)
      else
        Result.new(start_time + timeout - Concurrent.monotonic_time)
      end
    end

    # Wakes up a waiting thread
    # @return [true]
    def signal
      @condition.signal
      true
    end

    # Wakes up all waiting threads
    # @return [true]
    def broadcast
      @condition.broadcast
      true
    end
  end
end
