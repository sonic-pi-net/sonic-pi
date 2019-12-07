require 'mocha/parameter_matchers/base'

module Mocha

  module ParameterMatchers

    # Matches any object that matches +regexp+.
    #
    # @param [Regexp] regexp regular expression to match.
    # @return [RegexpMatches] parameter matcher.
    #
    # @see Expectation#with
    #
    # @example Actual parameter is matched by specified regular expression.
    #   object = mock()
    #   object.expects(:method_1).with(regexp_matches(/e/))
    #   object.method_1('hello')
    #   # no error raised
    #
    # @example Actual parameter is not matched by specified regular expression.
    #   object = mock()
    #   object.expects(:method_1).with(regexp_matches(/a/))
    #   object.method_1('hello')
    #   # error raised, because method_1 was not called with a parameter that matched the
    #   # regular expression
    def regexp_matches(regexp)
      RegexpMatches.new(regexp)
    end

    # Parameter matcher which matches if specified regular expression matches actual paramter.
    class RegexpMatches < Base

      # @private
      def initialize(regexp)
        @regexp = regexp
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        return false unless parameter.respond_to?(:=~)
        parameter =~ @regexp
      end

      # @private
      def mocha_inspect
        "regexp_matches(#{@regexp.mocha_inspect})"
      end

    end

  end

end
