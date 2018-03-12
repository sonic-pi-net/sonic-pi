require 'mocha/method_matcher'
require 'mocha/parameters_matcher'
require 'mocha/expectation_error'
require 'mocha/return_values'
require 'mocha/exception_raiser'
require 'mocha/thrower'
require 'mocha/yield_parameters'
require 'mocha/is_a'
require 'mocha/in_state_ordering_constraint'
require 'mocha/change_state_side_effect'
require 'mocha/cardinality'

module Mocha

  # Methods on expectations returned from {Mock#expects}, {Mock#stubs}, {ObjectMethods#expects} and {ObjectMethods#stubs}.
  class Expectation

    # Modifies expectation so that the number of calls to the expected method must be within a specific +range+.
    #
    # @param [Range,Integer] range specifies the allowable range in the number of expected invocations.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Specifying a specific number of expected invocations.
    #   object = mock()
    #   object.expects(:expected_method).times(3)
    #   3.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).times(3)
    #   2.times { object.expected_method }
    #   # => verify fails
    #
    # @example Specifying a range in the number of expected invocations.
    #   object = mock()
    #   object.expects(:expected_method).times(2..4)
    #   3.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).times(2..4)
    #   object.expected_method
    #   # => verify fails
    def times(range)
      @cardinality = Cardinality.times(range)
      self
    end

    # Modifies expectation so that the expected method must be called exactly twice.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be invoked exactly twice.
    #   object = mock()
    #   object.expects(:expected_method).twice
    #   object.expected_method
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).twice
    #   object.expected_method
    #   object.expected_method
    #   object.expected_method # => unexpected invocation
    #
    #   object = mock()
    #   object.expects(:expected_method).twice
    #   object.expected_method
    #   # => verify fails
    def twice
      @cardinality = Cardinality.exactly(2)
      self
    end

    # Modifies expectation so that the expected method must be called exactly once.
    #
    # Note that this is the default behaviour for an expectation, but you may wish to use it for clarity/emphasis.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be invoked exactly once.
    #   object = mock()
    #   object.expects(:expected_method).once
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).once
    #   object.expected_method
    #   object.expected_method # => unexpected invocation
    #
    #   object = mock()
    #   object.expects(:expected_method).once
    #   # => verify fails
    def once
      @cardinality = Cardinality.exactly(1)
      self
    end

    # Modifies expectation so that the expected method must never be called.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must never be called.
    #   object = mock()
    #   object.expects(:expected_method).never
    #   object.expected_method # => unexpected invocation
    #
    #   object = mock()
    #   object.expects(:expected_method).never
    #   # => verify succeeds
    def never
      @cardinality = Cardinality.exactly(0)
      self
    end

    # Modifies expectation so that the expected method must be called at least a +minimum_number_of_times+.
    #
    # @param [Integer] minimum_number_of_times minimum number of expected invocations.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at least twice.
    #   object = mock()
    #   object.expects(:expected_method).at_least(2)
    #   3.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_least(2)
    #   object.expected_method
    #   # => verify fails
    def at_least(minimum_number_of_times)
      @cardinality = Cardinality.at_least(minimum_number_of_times)
      self
    end

    # Modifies expectation so that the expected method must be called at least once.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at least once.
    #   object = mock()
    #   object.expects(:expected_method).at_least_once
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_least_once
    #   # => verify fails
    def at_least_once
      at_least(1)
      self
    end

    # Modifies expectation so that the expected method must be called at most a +maximum_number_of_times+.
    #
    # @param [Integer] maximum_number_of_times maximum number of expected invocations.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at most twice.
    #   object = mock()
    #   object.expects(:expected_method).at_most(2)
    #   2.times { object.expected_method }
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_most(2)
    #   3.times { object.expected_method } # => unexpected invocation
    def at_most(maximum_number_of_times)
      @cardinality = Cardinality.at_most(maximum_number_of_times)
      self
    end

    # Modifies expectation so that the expected method must be called at most once.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called at most once.
    #   object = mock()
    #   object.expects(:expected_method).at_most_once
    #   object.expected_method
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).at_most_once
    #   2.times { object.expected_method } # => unexpected invocation
    def at_most_once()
      at_most(1)
      self
    end

    # Modifies expectation so that the expected method must be called with +expected_parameters+.
    #
    # May be used with parameter matchers in {ParameterMatchers}.
    #
    # @param [*Array] expected_parameters parameters expected.
    # @yield optional block specifying custom matching.
    # @yieldparam [*Array] actual_parameters parameters with which expected method was invoked.
    # @yieldreturn [Boolean] +true+ if +actual_parameters+ are acceptable.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Expected method must be called with expected parameters.
    #   object = mock()
    #   object.expects(:expected_method).with(:param1, :param2)
    #   object.expected_method(:param1, :param2)
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).with(:param1, :param2)
    #   object.expected_method(:param3)
    #   # => verify fails
    #
    # @example Expected method must be called with a value divisible by 4.
    #   object = mock()
    #   object.expects(:expected_method).with() { |value| value % 4 == 0 }
    #   object.expected_method(16)
    #   # => verify succeeds
    #
    #   object = mock()
    #   object.expects(:expected_method).with() { |value| value % 4 == 0 }
    #   object.expected_method(17)
    #   # => verify fails
    def with(*expected_parameters, &matching_block)
      @parameters_matcher = ParametersMatcher.new(expected_parameters, &matching_block)
      self
    end

    # Modifies expectation so that when the expected method is called, it yields with the specified +parameters+.
    #
    # May be called multiple times on the same expectation for consecutive invocations.
    #
    # @param [*Array] parameters parameters to be yielded.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    # @see #then
    #
    # @example Yield parameters when expected method is invoked.
    #   object = mock()
    #   object.expects(:expected_method).yields('result')
    #   yielded_value = nil
    #   object.expected_method { |value| yielded_value = value }
    #   yielded_value # => 'result'
    #
    # @example Yield different parameters on different invocations of the expected method.
    #   object = mock()
    #   object.stubs(:expected_method).yields(1).then.yields(2)
    #   yielded_values_from_first_invocation = []
    #   yielded_values_from_second_invocation = []
    #   object.expected_method { |value| yielded_values_from_first_invocation << value } # first invocation
    #   object.expected_method { |value| yielded_values_from_second_invocation << value } # second invocation
    #   yielded_values_from_first_invocation # => [1]
    #   yielded_values_from_second_invocation # => [2]
    def yields(*parameters)
      @yield_parameters.add(*parameters)
      self
    end

    # Modifies expectation so that when the expected method is called, it yields multiple times per invocation with the specified +parameter_groups+.
    #
    # @param [*Array<Array>] parameter_groups each element of +parameter_groups+ should iself be an +Array+ representing the parameters to be passed to the block for a single yield.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    # @see #then
    #
    # @example When the +expected_method+ is called, the stub will invoke the block twice, the first time it passes +'result_1'+, +'result_2'+ as the parameters, and the second time it passes 'result_3' as the parameters.
    #   object = mock()
    #   object.expects(:expected_method).multiple_yields(['result_1', 'result_2'], ['result_3'])
    #   yielded_values = []
    #   object.expected_method { |*values| yielded_values << values }
    #   yielded_values # => [['result_1', 'result_2'], ['result_3]]
    #
    # @example Yield different groups of parameters on different invocations of the expected method.
    #   object = mock()
    #   object.stubs(:expected_method).multiple_yields([1, 2], [3]).then.multiple_yields([4], [5, 6])
    #   yielded_values_from_first_invocation = []
    #   yielded_values_from_second_invocation = []
    #   object.expected_method { |*values| yielded_values_from_first_invocation << values } # first invocation
    #   object.expected_method { |*values| yielded_values_from_second_invocation << values } # second invocation
    #   yielded_values_from_first_invocation # => [[1, 2], [3]]
    #   yielded_values_from_second_invocation # => [[4], [5, 6]]
    def multiple_yields(*parameter_groups)
      @yield_parameters.multiple_add(*parameter_groups)
      self
    end

    # Modifies expectation so that when the expected method is called, it returns the specified +value+.
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    # @see #then
    #
    # @overload def returns(value)
    #   @param [Object] value value to return on invocation of expected method.
    # @overload def returns(*values)
    #   @param [*Array] values values to return on consecutive invocations of expected method.
    #
    # @example Return the same value on every invocation.
    #   object = mock()
    #   object.stubs(:stubbed_method).returns('result')
    #   object.stubbed_method # => 'result'
    #   object.stubbed_method # => 'result'
    #
    # @example Return a different value on consecutive invocations.
    #   object = mock()
    #   object.stubs(:stubbed_method).returns(1, 2)
    #   object.stubbed_method # => 1
    #   object.stubbed_method # => 2
    #
    # @example Alternative way to return a different value on consecutive invocations.
    #   object = mock()
    #   object.stubs(:expected_method).returns(1, 2).then.returns(3)
    #   object.expected_method # => 1
    #   object.expected_method # => 2
    #   object.expected_method # => 3
    #
    # @example May be called in conjunction with {#raises} on the same expectation.
    #   object = mock()
    #   object.stubs(:expected_method).returns(1, 2).then.raises(Exception)
    #   object.expected_method # => 1
    #   object.expected_method # => 2
    #   object.expected_method # => raises exception of class Exception1
    #
    # @example Note that in Ruby a method returning multiple values is exactly equivalent to a method returning an +Array+ of those values.
    #   object = mock()
    #   object.stubs(:expected_method).returns([1, 2])
    #   x, y = object.expected_method
    #   x # => 1
    #   y # => 2
    def returns(*values)
      @return_values += ReturnValues.build(*values)
      self
    end

    # Modifies expectation so that when the expected method is called, it raises the specified +exception+ with the specified +message+ i.e. calls +Kernel#raise(exception, message)+.
    #
    # @param [Class,Exception,String,#exception] exception exception to be raised or message to be passed to RuntimeError.
    # @param [String] message exception message.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see Kernel#raise
    # @see #then
    #
    # @overload def raises
    # @overload def raises(exception)
    # @overload def raises(exception, message)
    #
    # @example Raise specified exception if expected method is invoked.
    #   object = stub()
    #   object.stubs(:expected_method).raises(Exception, 'message')
    #   object.expected_method # => raises exception of class Exception and with message 'message'
    #
    # @example Raise custom exception with extra constructor parameters by passing in an instance of the exception.
    #   object = stub()
    #   object.stubs(:expected_method).raises(MyException.new('message', 1, 2, 3))
    #   object.expected_method # => raises the specified instance of MyException
    #
    # @example Raise different exceptions on consecutive invocations of the expected method.
    #   object = stub()
    #   object.stubs(:expected_method).raises(Exception1).then.raises(Exception2)
    #   object.expected_method # => raises exception of class Exception1
    #   object.expected_method # => raises exception of class Exception2
    #
    # @example Raise an exception on first invocation of expected method and then return values on subsequent invocations.
    #   object = stub()
    #   object.stubs(:expected_method).raises(Exception).then.returns(2, 3)
    #   object.expected_method # => raises exception of class Exception1
    #   object.expected_method # => 2
    #   object.expected_method # => 3
    def raises(exception = RuntimeError, message = nil)
      @return_values += ReturnValues.new(ExceptionRaiser.new(exception, message))
      self
    end

    # Modifies expectation so that when the expected method is called, it throws the specified +tag+ with the specific return value +object+ i.e. calls +Kernel#throw(tag, object)+.
    #
    # @param [Symbol,String] tag tag to throw to transfer control to the active catch block.
    # @param [Object] object return value for the catch block.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see Kernel#throw
    # @see #then
    #
    # @overload def throw(tag)
    # @overload def throw(tag, object)
    #
    # @example Throw tag when expected method is invoked.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done)
    #   object.expected_method # => throws tag :done
    #
    # @example Throw tag with return value +object+ c.f. +Kernel#throw+.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done, 'result')
    #   object.expected_method # => throws tag :done and causes catch block to return 'result'
    #
    # @example Throw different tags on consecutive invocations of the expected method.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done).then.throws(:continue)
    #   object.expected_method # => throws :done
    #   object.expected_method # => throws :continue
    #
    # @example Throw tag on first invocation of expected method and then return values for subsequent invocations.
    #   object = stub()
    #   object.stubs(:expected_method).throws(:done).then.returns(2, 3)
    #   object.expected_method # => throws :done
    #   object.expected_method # => 2
    #   object.expected_method # => 3
    def throws(tag, object = nil)
      @return_values += ReturnValues.new(Thrower.new(tag, object))
      self
    end

    # @overload def then
    #   Used as syntactic sugar to improve readability. It has no effect on state of the expectation.
    # @overload def then(state_machine.is(state_name))
    #   Used to change the +state_machine+ to the state specified by +state_name+ when the expected invocation occurs.
    #   @param [StateMachine::State] state_machine.is(state_name) provides a mechanism to change the +state_machine+ into the state specified by +state_name+ when the expected method is invoked.
    #
    #   @see API#states
    #   @see StateMachine
    #   @see #when
    #
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @example Using {#then} as syntactic sugar when specifying values to be returned and exceptions to be raised on consecutive invocations of the expected method.
    #   object = mock()
    #   object.stubs(:expected_method).returns(1, 2).then.raises(Exception).then.returns(4)
    #   object.expected_method # => 1
    #   object.expected_method # => 2
    #   object.expected_method # => raises exception of class Exception
    #   object.expected_method # => 4
    #
    # @example Using {#then} to change the +state+ of a +state_machine+ on the invocation of an expected method.
    #   power = states('power').starts_as('off')
    #
    #   radio = mock('radio')
    #   radio.expects(:switch_on).then(power.is('on'))
    #   radio.expects(:select_channel).with('BBC Radio 4').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(+5).when(power.is('on'))
    #   radio.expects(:select_channel).with('BBC World Service').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(-5).when(power.is('on'))
    #   radio.expects(:switch_off).then(power.is('off'))
    def then(*parameters)
      if parameters.length == 1
        state = parameters.first
        add_side_effect(ChangeStateSideEffect.new(state))
      end
      self
    end

    # Constrains the expectation to occur only when the +state_machine+ is in the state specified by +state_name+.
    #
    # @param [StateMachine::StatePredicate] state_machine.is(state_name) provides a mechanism to determine whether the +state_machine+ is in the state specified by +state_name+ when the expected method is invoked.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see API#states
    # @see StateMachine
    # @see #then
    #
    # @example Using {#when} to only allow invocation of methods when "power" state machine is in the "on" state.
    #   power = states('power').starts_as('off')
    #
    #   radio = mock('radio')
    #   radio.expects(:switch_on).then(power.is('on'))
    #   radio.expects(:select_channel).with('BBC Radio 4').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(+5).when(power.is('on'))
    #   radio.expects(:select_channel).with('BBC World Service').when(power.is('on'))
    #   radio.expects(:adjust_volume).with(-5).when(power.is('on'))
    #   radio.expects(:switch_off).then(power.is('off'))
    def when(state_predicate)
      add_ordering_constraint(InStateOrderingConstraint.new(state_predicate))
      self
    end

    # Constrains the expectation so that it must be invoked at the current point in the +sequence+.
    #
    # To expect a sequence of invocations, write the expectations in order and add the +in_sequence(sequence)+ clause to each one.
    #
    # Expectations in a +sequence+ can have any invocation count.
    #
    # If an expectation in a sequence is stubbed, rather than expected, it can be skipped in the +sequence+.
    #
    # An expected method can appear in multiple sequences.
    #
    # @param [*Array<Sequence>] sequences sequences in which expected method should appear.
    # @return [Expectation] the same expectation, thereby allowing invocations of other {Expectation} methods to be chained.
    #
    # @see API#sequence
    #
    # @example Ensure methods are invoked in a specified order.
    #   breakfast = sequence('breakfast')
    #
    #   egg = mock('egg')
    #   egg.expects(:crack).in_sequence(breakfast)
    #   egg.expects(:fry).in_sequence(breakfast)
    #   egg.expects(:eat).in_sequence(breakfast)
    def in_sequence(*sequences)
      sequences.each { |sequence| add_in_sequence_ordering_constraint(sequence) }
      self
    end

    # @private
    attr_reader :backtrace

    # @private
    def initialize(mock, expected_method_name, backtrace = nil)
      @mock = mock
      @method_matcher = MethodMatcher.new(expected_method_name.to_sym)
      @parameters_matcher = ParametersMatcher.new
      @ordering_constraints = []
      @side_effects = []
      @cardinality, @invocation_count = Cardinality.exactly(1), 0
      @return_values = ReturnValues.new
      @yield_parameters = YieldParameters.new
      @backtrace = backtrace || caller
    end

    # @private
    def add_ordering_constraint(ordering_constraint)
      @ordering_constraints << ordering_constraint
    end

    # @private
    def add_in_sequence_ordering_constraint(sequence)
      sequence.constrain_as_next_in_sequence(self)
    end

    # @private
    def add_side_effect(side_effect)
      @side_effects << side_effect
    end

    # @private
    def perform_side_effects
      @side_effects.each { |side_effect| side_effect.perform }
    end

    # @private
    def in_correct_order?
      @ordering_constraints.all? { |ordering_constraint| ordering_constraint.allows_invocation_now? }
    end

    # @private
    def matches_method?(method_name)
      @method_matcher.match?(method_name)
    end

    # @private
    def match?(actual_method_name, *actual_parameters)
      @method_matcher.match?(actual_method_name) && @parameters_matcher.match?(actual_parameters) && in_correct_order?
    end

    # @private
    def invocations_allowed?
      @cardinality.invocations_allowed?(@invocation_count)
    end

    # @private
    def satisfied?
      @cardinality.satisfied?(@invocation_count)
    end

    # @private
    def invoke
      @invocation_count += 1
      perform_side_effects()
      if block_given? then
        @yield_parameters.next_invocation.each do |yield_parameters|
          yield(*yield_parameters)
        end
      end
      @return_values.next
    end

    # @private
    def verified?(assertion_counter = nil)
      assertion_counter.increment if assertion_counter && @cardinality.needs_verifying?
      @cardinality.verified?(@invocation_count)
    end

    # @private
    def used?
      @cardinality.used?(@invocation_count)
    end

    # @private
    def inspect
      address = __id__ * 2
      address += 0x100000000 if address < 0
      "#<Expectation:0x#{'%x' % address} #{mocha_inspect} >"
    end

    # @private
    def mocha_inspect
      message = "#{@cardinality.mocha_inspect}, "
      message << case @invocation_count
        when 0 then "not yet invoked"
        when 1 then "invoked once"
        when 2 then "invoked twice"
        else "invoked #{@invocation_count} times"
      end
      message << ": "
      message << method_signature
      message << "; #{@ordering_constraints.map { |oc| oc.mocha_inspect }.join("; ")}" unless @ordering_constraints.empty?
      message
    end

    # @private
    def method_signature
      "#{@mock.mocha_inspect}.#{@method_matcher.mocha_inspect}#{@parameters_matcher.mocha_inspect}"
    end

  end

end
