require 'mocha/parameter_matchers/base'

module Mocha

  module ParameterMatchers

    # Matches any object that is a +klass+.
    #
    # @param [Class] klass expected class.
    # @return [IsA] parameter matcher.
    #
    # @see Expectation#with
    # @see Kernel#is_a?
    #
    # @example Actual parameter is a +Integer+.
    #   object = mock()
    #   object.expects(:method_1).with(is_a(Integer))
    #   object.method_1(99)
    #   # no error raised
    #
    # @example Actual parameter is not a +Integer+.
    #   object = mock()
    #   object.expects(:method_1).with(is_a(Integer))
    #   object.method_1('string')
    #   # error raised, because method_1 was not called with an Integer
    def is_a(klass)
      IsA.new(klass)
    end

    # Parameter matcher which matches when actual parameter is a specific class.
    class IsA < Base

      # @private
      def initialize(klass)
        @klass = klass
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        parameter.is_a?(@klass)
      end

      # @private
      def mocha_inspect
        "is_a(#{@klass.mocha_inspect})"
      end

    end

  end

end
