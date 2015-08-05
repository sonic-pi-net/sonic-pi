require 'concurrent/utility/native_extension_loader'
require 'concurrent/synchronization'

module Concurrent

  # @!macro [attach] atomic_fixnum
  #
  #   A numeric value that can be updated atomically. Reads and writes to an atomic
  #   fixnum and thread-safe and guaranteed to succeed. Reads and writes may block
  #   briefly but no explicit locking is required.
  #
  #       Testing with ruby 2.1.2
  #       Testing with Concurrent::MutexAtomicFixnum...
  #         3.130000   0.000000   3.130000 (  3.136505)
  #       Testing with Concurrent::CAtomicFixnum...
  #         0.790000   0.000000   0.790000 (  0.785550)
  #
  #       Testing with jruby 1.9.3
  #       Testing with Concurrent::MutexAtomicFixnum...
  #         5.460000   2.460000   7.920000 (  3.715000)
  #       Testing with Concurrent::JavaAtomicFixnum...
  #         4.520000   0.030000   4.550000 (  1.187000)
  #
  #   @see http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/atomic/AtomicLong.html java.util.concurrent.atomic.AtomicLong
  #
  # @!visibility private
  # @!macro internal_implementation_note
  class MutexAtomicFixnum < Synchronization::Object

    # http://stackoverflow.com/questions/535721/ruby-max-integer
    MIN_VALUE = -(2**(0.size * 8 - 2))
    MAX_VALUE = (2**(0.size * 8 - 2) - 1)

    # @!macro [attach] atomic_fixnum_method_initialize
    #
    #   Creates a new `AtomicFixnum` with the given initial value.
    #
    #   @param [Fixnum] initial the initial value
    #   @raise [ArgumentError] if the initial value is not a `Fixnum`
    def initialize(initial = 0)
      super()
      synchronize { ns_initialize(initial) }
    end

    # @!macro [attach] atomic_fixnum_method_value_get
    #
    #   Retrieves the current `Fixnum` value.
    #
    #   @return [Fixnum] the current value
    def value
      synchronize { @value }
    end

    # @!macro [attach] atomic_fixnum_method_value_set
    #
    #   Explicitly sets the value.
    #
    #   @param [Fixnum] value the new value to be set
    #
    #   @return [Fixnum] the current value
    #
    #   @raise [ArgumentError] if the new value is not a `Fixnum`
    def value=(value)
      synchronize { ns_set(value) }
    end

    # @!macro [attach] atomic_fixnum_method_increment
    #
    #   Increases the current value by 1.
    #
    #   @return [Fixnum] the current value after incrementation
    def increment
      synchronize { ns_set(@value + 1) }
    end

    alias_method :up, :increment

    # @!macro [attach] atomic_fixnum_method_decrement
    #
    #   Decreases the current value by 1.
    #
    #   @return [Fixnum] the current value after decrementation
    def decrement
      synchronize { ns_set(@value -1) }
    end

    alias_method :down, :decrement

    # @!macro [attach] atomic_fixnum_method_compare_and_set
    #
    #   Atomically sets the value to the given updated value if the current
    #   value == the expected value.
    #
    #   @param [Fixnum] expect the expected value
    #   @param [Fixnum] update the new value
    #
    #   @return [Fixnum] true if the value was updated else false
    def compare_and_set(expect, update)
      synchronize do
        if @value == expect
          @value = update
          true
        else
          false
        end
      end
    end

    protected

    # @!visibility private
    def ns_initialize(initial)
      ns_set(initial)
    end

    private

    # @!visibility private
    def ns_set(value)
      range_check!(value)
      @value = value
    end

    # @!visibility private
    def range_check!(value)
      if !value.is_a?(Fixnum)
        raise ArgumentError.new('value value must be a Fixnum')
      elsif value > MAX_VALUE
        raise RangeError.new("#{value} is greater than the maximum value of #{MAX_VALUE}")
      elsif value < MIN_VALUE
        raise RangeError.new("#{value} is less than the maximum value of #{MIN_VALUE}")
      else
        value
      end
    end
  end

  # @!visibility private
  # @!macro internal_implementation_note
  AtomicFixnumImplementation = case
                               when Concurrent.on_jruby?
                                 JavaAtomicFixnum
                               when defined?(CAtomicFixnum)
                                 CAtomicFixnum
                               else
                                 MutexAtomicFixnum
                               end
  private_constant :AtomicFixnumImplementation

  # @!macro atomic_fixnum
  #
  # @see Concurrent::MutexAtomicFixnum
  class AtomicFixnum < AtomicFixnumImplementation

    # @!method initialize(initial = 0)
    #   @!macro atomic_fixnum_method_initialize

    # @!method value
    #   @!macro atomic_fixnum_method_value_get

    # @!method value=(value)
    #   @!macro atomic_fixnum_method_value_set

    # @!method increment
    #   @!macro atomic_fixnum_method_increment

    # @!method decrement
    #   @!macro atomic_fixnum_method_decrement

    # @!method compare_and_set(expect, update)
    #   @!macro atomic_fixnum_method_compare_and_set

  end
end
