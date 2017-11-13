require 'mocha/parameter_matchers'
require 'mocha/hooks'
require 'mocha/mockery'
require 'mocha/sequence'
require 'mocha/object_methods'
require 'mocha/module_methods'
require 'mocha/class_methods'

module Mocha

  # Methods added to +Test::Unit::TestCase+, +MiniTest::Unit::TestCase+ or equivalent.
  module API

    include ParameterMatchers
    include Hooks

    # @private
    def self.included(mod)
      Object.send(:include, Mocha::ObjectMethods)
      Module.send(:include, Mocha::ModuleMethods)
      Class.send(:include, Mocha::ClassMethods)
    end

    # Builds a new mock object
    #
    # @param [String] name identifies mock object in error messages.
    # @param [Hash] expected_methods_vs_return_values expected method name symbols as keys and corresponding return values as values - these expectations are setup as if {Mock#expects} were called multiple times.
    # @yield optional block to be evaluated against the mock object instance, giving an alternative way to setup expectations.
    # @return [Mock] a new mock object
    #
    # @overload def mock(name, &block)
    # @overload def mock(expected_methods_vs_return_values = {}, &block)
    # @overload def mock(name, expected_methods_vs_return_values = {}, &block)
    #
    # @example Using expected_methods_vs_return_values Hash to setup expectations.
    #   def test_motor_starts_and_stops
    #     motor = mock('motor', :start => true, :stop => true)
    #     assert motor.start
    #     assert motor.stop
    #     # an error will be raised unless both Motor#start and Motor#stop have been called
    #   end
    # @example Using the optional block to setup expectations & stubbed methods.
    #   def test_motor_starts_and_stops
    #     motor = mock('motor') do
    #       expects(:start).with(100.rpm).returns(true)
    #       stubs(:stop).returns(true)
    #     end
    #     assert motor.start(100.rpm)
    #     assert motor.stop
    #     # an error will only be raised if Motor#start(100.rpm) has not been called
    #   end
    def mock(*arguments, &block)
      name = arguments.shift if arguments.first.is_a?(String)
      expectations = arguments.shift || {}
      mock = name ? Mockery.instance.named_mock(name, &block) : Mockery.instance.unnamed_mock(&block)
      mock.expects(expectations)
      mock
    end

    # Builds a new mock object
    #
    # @param [String] name identifies mock object in error messages.
    # @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {Mock#stubs} were called multiple times.
    # @yield optional block to be evaluated against the mock object instance, giving an alternative way to setup stubbed methods.
    # @return [Mock] a new mock object
    #
    # @overload def stub(name, &block)
    # @overload def stub(stubbed_methods_vs_return_values = {}, &block)
    # @overload def stub(name, stubbed_methods_vs_return_values = {}, &block)
    #
    # @example Using stubbed_methods_vs_return_values Hash to setup stubbed methods.
    #   def test_motor_starts_and_stops
    #     motor = mock('motor', :start => true, :stop => true)
    #     assert motor.start
    #     assert motor.stop
    #     # an error will not be raised even if either Motor#start or Motor#stop has not been called
    #   end
    #
    # @example Using the optional block to setup expectations & stubbed methods.
    #   def test_motor_starts_and_stops
    #     motor = mock('motor') do
    #       expects(:start).with(100.rpm).returns(true)
    #       stubs(:stop).returns(true)
    #     end
    #     assert motor.start(100.rpm)
    #     assert motor.stop
    #     # an error will only be raised if Motor#start(100.rpm) has not been called
    #   end
    def stub(*arguments, &block)
      name = arguments.shift if arguments.first.is_a?(String)
      expectations = arguments.shift || {}
      stub = name ? Mockery.instance.named_mock(name, &block) : Mockery.instance.unnamed_mock(&block)
      stub.stubs(expectations)
      stub
    end

    # Builds a mock object that accepts calls to any method. By default it will return +nil+ for any method call.
    #
    # @param [String] name identifies mock object in error messages.
    # @param [Hash] stubbed_methods_vs_return_values stubbed method name symbols as keys and corresponding return values as values - these stubbed methods are setup as if {Mock#stubs} were called multiple times.
    # @yield optional block to be evaluated against the mock object instance, giving an alternative way to setup stubbed methods.
    # @return [Mock] a new mock object
    #
    # @overload def stub_everything(name, &block)
    # @overload def stub_everything(stubbed_methods_vs_return_values = {}, &block)
    # @overload def stub_everything(name, stubbed_methods_vs_return_values = {}, &block)
    #
    # @example Ignore invocations of irrelevant methods.
    #   def test_motor_stops
    #     motor = stub_everything('motor', :stop => true)
    #     assert_nil motor.irrelevant_method_1 # => no error raised
    #     assert_nil motor.irrelevant_method_2 # => no error raised
    #     assert motor.stop
    #   end
    def stub_everything(*arguments, &block)
      name = arguments.shift if arguments.first.is_a?(String)
      expectations = arguments.shift || {}
      stub = name ? Mockery.instance.named_mock(name, &block) : Mockery.instance.unnamed_mock(&block)
      stub.stub_everything
      stub.stubs(expectations)
      stub
    end

    # Builds a new sequence which can be used to constrain the order in which expectations can occur.
    #
    # Specify that an expected invocation must occur within a named {Sequence} by using {Expectation#in_sequence}.
    #
    # @return [Sequence] a new sequence
    #
    # @see Expectation#in_sequence
    #
    # @example Ensure methods on egg are invoked in correct order.
    #   breakfast = sequence('breakfast')
    #
    #   egg = mock('egg') do
    #     expects(:crack).in_sequence(breakfast)
    #     expects(:fry).in_sequence(breakfast)
    #     expects(:eat).in_sequence(breakfast)
    #   end
    def sequence(name)
      Sequence.new(name)
    end

    # Builds a new state machine which can be used to constrain the order in which expectations can occur.
    #
    # Specify the initial state of the state machine by using {StateMachine#starts_as}.
    #
    # Specify that an expected invocation should change the state of the state machine by using {Expectation#then}.
    #
    # Specify that an expected invocation should be constrained to occur within a particular +state+ by using {Expectation#when}.
    #
    # A test can contain multiple state machines.
    #
    # @return [StateMachine] a new state machine
    #
    # @see Expectation#then
    # @see Expectation#when
    # @see StateMachine
    # @example Constrain expected invocations to occur in particular states.
    #   power = states('power').starts_as('off')
    #
    #   radio = mock('radio') do
    #     expects(:switch_on).then(power.is('on'))
    #     expects(:select_channel).with('BBC Radio 4').when(power.is('on'))
    #     expects(:adjust_volume).with(+5).when(power.is('on'))
    #     expects(:select_channel).with('BBC World Service').when(power.is('on'))
    #     expects(:adjust_volume).with(-5).when(power.is('on'))
    #     expects(:switch_off).then(power.is('off'))
    #   end
    def states(name)
      Mockery.instance.new_state_machine(name)
    end

  end

end
