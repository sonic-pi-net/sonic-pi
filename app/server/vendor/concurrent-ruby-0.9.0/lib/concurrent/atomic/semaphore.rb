require 'concurrent/synchronization'

module Concurrent

  # @!macro [attach] semaphore
  #
  #   A counting semaphore. Conceptually, a semaphore maintains a set of
  #   permits. Each {#acquire} blocks if necessary until a permit is
  #   available, and then takes it. Each {#release} adds a permit, potentially
  #   releasing a blocking acquirer.
  #   However, no actual permit objects are used; the Semaphore just keeps a
  #   count of the number available and acts accordingly.
  #
  # @!visibility private
  #
  # @!macro internal_implementation_note
  class MutexSemaphore < Synchronization::Object

    # @!macro [attach] semaphore_method_initialize
    #
    #   Create a new `Semaphore` with the initial `count`.
    #
    #   @param [Fixnum] count the initial count
    #
    #   @raise [ArgumentError] if `count` is not an integer or is less than zero
    def initialize(count)
      unless count.is_a?(Fixnum) && count >= 0
        fail ArgumentError, 'count must be an non-negative integer'
      end
      super()
      synchronize { ns_initialize count }
    end

    # @!macro [attach] semaphore_method_acquire
    #
    #   Acquires the given number of permits from this semaphore,
    #     blocking until all are available.
    #
    #   @param [Fixnum] permits Number of permits to acquire
    #
    #   @raise [ArgumentError] if `permits` is not an integer or is less than
    #     one
    #
    #   @return [Nil]
    def acquire(permits = 1)
      unless permits.is_a?(Fixnum) && permits > 0
        fail ArgumentError, 'permits must be an integer greater than zero'
      end
      synchronize do
        try_acquire_timed(permits, nil)
        nil
      end
    end

    # @!macro [attach] semaphore_method_available_permits
    #
    #   Returns the current number of permits available in this semaphore.
    #
    #   @return [Integer]
    def available_permits
      synchronize { @free }
    end

    # @!macro [attach] semaphore_method_drain_permits
    #
    #   Acquires and returns all permits that are immediately available.
    #
    #   @return [Integer]
    def drain_permits
      synchronize do
        @free.tap { |_| @free = 0 }
      end
    end

    # @!macro [attach] semaphore_method_try_acquire
    #
    #   Acquires the given number of permits from this semaphore,
    #     only if all are available at the time of invocation or within
    #     `timeout` interval
    #
    #   @param [Fixnum] permits the number of permits to acquire
    #
    #   @param [Fixnum] timeout the number of seconds to wait for the counter
    #     or `nil` to return immediately
    #
    #   @raise [ArgumentError] if `permits` is not an integer or is less than
    #     one
    #
    #   @return [Boolean] `false` if no permits are available, `true` when
    #     acquired a permit
    def try_acquire(permits = 1, timeout = nil)
      unless permits.is_a?(Fixnum) && permits > 0
        fail ArgumentError, 'permits must be an integer greater than zero'
      end
      synchronize do
        if timeout.nil?
          try_acquire_now(permits)
        else
          try_acquire_timed(permits, timeout)
        end
      end
    end

    # @!macro [attach] semaphore_method_release
    #
    #   Releases the given number of permits, returning them to the semaphore.
    #
    #   @param [Fixnum] permits Number of permits to return to the semaphore.
    #
    #   @raise [ArgumentError] if `permits` is not a number or is less than one
    #
    #   @return [Nil]
    def release(permits = 1)
      unless permits.is_a?(Fixnum) && permits > 0
        fail ArgumentError, 'permits must be an integer greater than zero'
      end
      synchronize do
        @free += permits
        permits.times { ns_signal }
      end
      nil
    end

    # @!macro [attach] semaphore_method_reduce_permits
    #
    #   @api private
    #
    #   Shrinks the number of available permits by the indicated reduction.
    #
    #   @param [Fixnum] reduction Number of permits to remove.
    #
    #   @raise [ArgumentError] if `reduction` is not an integer or is negative
    #
    #   @raise [ArgumentError] if `@free` - `@reduction` is less than zero
    #
    #   @return [Nil]
    def reduce_permits(reduction)
      unless reduction.is_a?(Fixnum) && reduction >= 0
        fail ArgumentError, 'reduction must be an non-negative integer'
      end
      synchronize { @free -= reduction }
      nil
    end

    protected

    # @!visibility private
    def ns_initialize(count)
      @free = count
    end

    private

    # @!visibility private
    def try_acquire_now(permits)
      if @free >= permits
        @free -= permits
        true
      else
        false
      end
    end

    # @!visibility private
    def try_acquire_timed(permits, timeout)
      ns_wait_until(timeout) { try_acquire_now(permits) }
    end
  end

  # @!visibility private
  # @!macro internal_implementation_note
  SemaphoreImplementation = case
                            when Concurrent.on_jruby?
                              JavaSemaphore
                            else
                              MutexSemaphore
                            end
  private_constant :SemaphoreImplementation

  # @!macro semaphore
  #
  # @see Concurrent::MutexSemaphore
  class Semaphore < SemaphoreImplementation

    # @!method initialize(count)
    #   @!macro semaphore_method_initialize

    # @!method acquire(permits = 1)
    #   @!macro semaphore_method_acquire

    # @!method available_permits
    #   @!macro semaphore_method_available_permits

    # @!method drain_permits
    #   @!macro semaphore_method_drain_permits

    # @!method try_acquire(permits = 1, timeout = nil)
    #   @!macro semaphore_method_try_acquire

    # @!method release(permits = 1)
    #   @!macro semaphore_method_release

    # @!method reduce_permits(reduction)
    #   @!macro semaphore_method_reduce_permits

  end
end
