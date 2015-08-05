require 'concurrent/concern/dereferenceable'
require 'concurrent/atomic/atomic_reference'
require 'concurrent/synchronization/object'

module Concurrent

  # Atoms provide a way to manage shared, synchronous, independent state.
  #
  # An atom is initialized with an initial value and an optional validation
  # proc. At any time the value of the atom can be synchronously and safely
  # changed. If a validator is given at construction then any new value
  # will be checked against the validator and will be rejected if the
  # validator returns false or raises an exception.
  #
  # There are two ways to change the value of an atom: {#compare_and_set} and
  # {#swap}. The former will set the new value if and only if it validates and
  # the current value matches the new value. The latter will atomically set the
  # new value to the result of running the given block if and only if that
  # value validates.
  #
  # @!macro copy_options
  #
  # @see http://clojure.org/atoms Clojure Atoms
  class Atom < Synchronization::Object
    include Concern::Dereferenceable

    # Create a new atom with the given initial value.
    #
    # @param [Object] value The initial value
    # @param [Hash] opts The options used to configure the atom
    # @option opts [Proc] :validator (nil) Optional proc used to validate new
    #   values. It must accept one and only one argument which will be the
    #   intended new value. The validator will return true if the new value
    #   is acceptable else return false (preferrably) or raise an exception.
    #
    # @!macro deref_options
    # 
    # @raise [ArgumentError] if the validator is not a `Proc` (when given)
    def initialize(value, opts = {})
      super()

      @validator = opts.fetch(:validator, ->(v){ true })
      raise ArgumentError.new('validator must be a proc') unless @validator.is_a? Proc

      @value = Concurrent::AtomicReference.new(value)
      ns_set_deref_options(opts)
      ensure_ivar_visibility!
    end

    # The current value of the atom.
    #
    # @return [Object] The current value.
    def value
      apply_deref_options(@value.value)
    end
    alias_method :deref, :value

    # Atomically swaps the value of atom using the given block. The current
    # value will be passed to the block, as will any arguments passed as
    # arguments to the function. The new value will be validated against the
    # (optional) validator proc given at construction. If validation fails the
    # value will not be changed.
    #
    # Internally, {#swap} reads the current value, applies the block to it, and
    # attempts to compare-and-set it in. Since another thread may have changed
    # the value in the intervening time, it may have to retry, and does so in a
    # spin loop. The net effect is that the value will always be the result of
    # the application of the supplied block to a current value, atomically.
    # However, because the block might be called multiple times, it must be free
    # of side effects.
    # 
    # @note The given block may be called multiple times, and thus should be free
    #   of side effects.
    #
    # @param [Object] args Zero or more arguments passed to the block.
    #
    # @yield [value, args] Calculates a new value for the atom based on the
    #   current value and any supplied agruments.
    # @yieldparam value [Object] The current value of the atom.
    # @yieldparam args [Object] All arguments passed to the function, in order.
    # @yieldreturn [Object] The intended new value of the atom.
    #
    # @return [Object] The final value of the atom after all operations and
    #   validations are complete.
    #
    # @raise [ArgumentError] When no block is given.
    def swap(*args)
      raise ArgumentError.new('no block given') unless block_given?

      begin
        loop do
          old_value = @value.value
          new_value = yield(old_value, *args)
          return old_value unless @validator.call(new_value)
          return new_value if compare_and_set!(old_value, new_value)
        end
      rescue
        return @value.value
      end
    end

    # @!macro [attach] atom_compare_and_set
    #   Atomically sets the value of atom to the new value if and only if the
    #   current value of the atom is identical to the old value and the new
    #   value successfully validates against the (optional) validator given
    #   at construction.
    #
    #   @param [Object] old_value The expected current value.
    #   @param [Object] new_value The intended new value.
    #
    #   @return [Boolean] True if the value is changed else false.
    def compare_and_set(old_value, new_value)
      compare_and_set!(old_value, new_value)
    rescue
      false
    end

    private

    # @!macro atom_compare_and_set
    # @raise [Exception] if the validator proc raises an exception
    # @!visibility private
    def compare_and_set!(old_value, new_value)
      if @validator.call(new_value) # may raise exception
        @value.compare_and_set(old_value, new_value)
      else
        false
      end
    end
  end
end
