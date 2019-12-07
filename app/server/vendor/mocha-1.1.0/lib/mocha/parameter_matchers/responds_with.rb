require 'mocha/parameter_matchers/base'
require 'yaml'

module Mocha

  module ParameterMatchers

    # Matches any object that responds to +message+ with +result+. To put it another way, it tests the quack, not the duck.
    #
    # @param [Symbol] message method to invoke.
    # @param [Object] result expected result of sending +message+.
    # @return [RespondsWith] parameter matcher.
    #
    # @see Expectation#with
    #
    # @example Actual parameter responds with "FOO" when :upcase is invoked.
    #   object = mock()
    #   object.expects(:method_1).with(responds_with(:upcase, "FOO"))
    #   object.method_1("foo")
    #   # no error raised, because "foo".upcase == "FOO"
    #
    # @example Actual parameter does not respond with "FOO" when :upcase is invoked.
    #   object = mock()
    #   object.expects(:method_1).with(responds_with(:upcase, "BAR"))
    #   object.method_1("foo")
    #   # error raised, because "foo".upcase != "BAR"
    def responds_with(message, result)
      RespondsWith.new(message, result)
    end

    # Parameter matcher which matches if actual parameter returns expected result when specified method is invoked.
    class RespondsWith < Base

      # @private
      def initialize(message, result)
        @message, @result = message, result
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        @result.to_matcher.matches?( [parameter.__send__(@message)] )
      end

      # @private
      def mocha_inspect
        "responds_with(#{@message.mocha_inspect}, #{@result.mocha_inspect})"
      end

    end

  end

end
