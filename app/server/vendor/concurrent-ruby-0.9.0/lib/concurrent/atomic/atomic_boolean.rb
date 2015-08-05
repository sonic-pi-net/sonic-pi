require 'concurrent/utility/native_extension_loader'
require 'concurrent/synchronization'

module Concurrent

  # @!macro [attach] atomic_boolean
  #
  #   A boolean value that can be updated atomically. Reads and writes to an atomic
  #   boolean and thread-safe and guaranteed to succeed. Reads and writes may block
  #   briefly but no explicit locking is required.
  #
  #       Testing with ruby 2.1.2
  #       Testing with Concurrent::MutexAtomicBoolean...
  #         2.790000   0.000000   2.790000 (  2.791454)
  #       Testing with Concurrent::CAtomicBoolean...
  #         0.740000   0.000000   0.740000 (  0.740206)
  #
  #       Testing with jruby 1.9.3
  #       Testing with Concurrent::MutexAtomicBoolean...
  #         5.240000   2.520000   7.760000 (  3.683000)
  #       Testing with Concurrent::JavaAtomicBoolean...
  #         3.340000   0.010000   3.350000 (  0.855000)
  #
  #   @see http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/atomic/AtomicBoolean.html java.util.concurrent.atomic.AtomicBoolean
  #
  # @!visibility private
  #
  # @!macro internal_implementation_note
  class MutexAtomicBoolean < Synchronization::Object

    # @!macro [attach] atomic_boolean_method_initialize
    #
    #   Creates a new `AtomicBoolean` with the given initial value.
    #
    #   @param [Boolean] initial the initial value
    def initialize(initial = false)
      super()
      synchronize { ns_initialize(initial) }
    end

    # @!macro [attach] atomic_boolean_method_value_get
    #
    #   Retrieves the current `Boolean` value.
    #
    #   @return [Boolean] the current value
    def value
      synchronize { @value }
    end

    # @!macro [attach] atomic_boolean_method_value_set
    #
    #   Explicitly sets the value.
    #
    #   @param [Boolean] value the new value to be set
    #
    #   @return [Boolean] the current value
    def value=(value)
      synchronize { @value = !!value }
    end

    # @!macro [attach] atomic_boolean_method_true_question
    #
    #   Is the current value `true`
    #
    #   @return [Boolean] true if the current value is `true`, else false
    def true?
      synchronize { @value }
    end

    # @!macro [attach] atomic_boolean_method_false_question
    #
    #   Is the current value `false`
    #
    #   @return [Boolean] true if the current value is `false`, else false
    def false?
      synchronize { !@value }
    end

    # @!macro [attach] atomic_boolean_method_make_true
    #
    #   Explicitly sets the value to true.
    #
    #   @return [Boolean] true is value has changed, otherwise false
    def make_true
      synchronize { ns_make_value(true) }
    end

    # @!macro [attach] atomic_boolean_method_make_false
    #
    #   Explicitly sets the value to false.
    #
    #   @return [Boolean] true is value has changed, otherwise false
    def make_false
      synchronize { ns_make_value(false) }
    end

    protected

    # @!visibility private
    def ns_initialize(initial)
      @value = !!initial
    end

    # @!visibility private
    def ns_make_value(value)
      old = @value
      @value = value
      old != @value
    end
  end

  # @!visibility private
  # @!macro internal_implementation_note
  AtomicBooleanImplementation = case
                                when Concurrent.on_jruby?
                                  JavaAtomicBoolean
                                when defined?(CAtomicBoolean)
                                  CAtomicBoolean
                                else
                                  MutexAtomicBoolean
                                end
  private_constant :AtomicBooleanImplementation

  # @!macro atomic_boolean
  #
  # @see Concurrent::MutexAtomicBoolean
  class AtomicBoolean < AtomicBooleanImplementation

    # @!method initialize(initial = false)
    #   @!macro atomic_boolean_method_initialize

    # @!method value
    #   @!macro atomic_boolean_method_value_get

    # @!method value=(value)
    #   @!macro atomic_boolean_method_value_set

    # @!method true?
    #   @!macro atomic_boolean_method_true_question

    # @!method false?
    #   @!macro atomic_boolean_method_false_question

    # @!method make_true
    #   @!macro atomic_boolean_method_make_true

    # @!method make_false
    #   @!macro atomic_boolean_method_make_false

  end
end
