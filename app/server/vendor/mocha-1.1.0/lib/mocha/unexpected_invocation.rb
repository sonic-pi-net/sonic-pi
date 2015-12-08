module Mocha

  # Exception raised when an unexpected method is invoked
  class UnexpectedInvocation

    # @private
    def initialize(mock, symbol, *arguments)
      @mock, @symbol, @arguments = mock, symbol, arguments
    end

    # @private
    def full_description
      method_matcher = MethodMatcher.new(@symbol)
      parameters_matcher = ParametersMatcher.new(@arguments)
      method_signature = "#{@mock.mocha_inspect}.#{method_matcher.mocha_inspect}#{parameters_matcher.mocha_inspect}"
      "unexpected invocation: #{method_signature}\n"
    end

    # @private
    def short_description
      "unexpected invocation: #{@symbol}(#{@arguments.join(', ')})"
    end

  end

end
