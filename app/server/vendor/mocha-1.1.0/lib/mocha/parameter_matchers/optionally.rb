module Mocha

  module ParameterMatchers

    # Matches optional parameters if available.
    #
    # @param [*Array<Base>] matchers matchers for optional parameters.
    # @return [Optionally] parameter matcher.
    #
    # @see Expectation#with
    #
    # @example Only the two required parameters are supplied and they both match their expected value.
    #   object = mock()
    #   object.expects(:method_1).with(1, 2, optionally(3, 4))
    #   object.method_1(1, 2)
    #   # no error raised
    #
    # @example Both required parameters and one of the optional parameters are supplied and they all match their expected value.
    #   object = mock()
    #   object.expects(:method_1).with(1, 2, optionally(3, 4))
    #   object.method_1(1, 2, 3)
    #   # no error raised
    #
    # @example Both required parameters and both of the optional parameters are supplied and they all match their expected value.
    #   object = mock()
    #   object.expects(:method_1).with(1, 2, optionally(3, 4))
    #   object.method_1(1, 2, 3, 4)
    #   # no error raised
    #
    # @example One of the actual optional parameters does not match the expected value.
    #   object = mock()
    #   object.expects(:method_1).with(1, 2, optionally(3, 4))
    #   object.method_1(1, 2, 3, 5)
    #   # error raised, because optional parameters did not match
    def optionally(*matchers)
      Optionally.new(*matchers)
    end

    # Parameter matcher which allows optional parameters to be specified.
    class Optionally < Base

      # @private
      def initialize(*parameters)
        @matchers = parameters.map { |parameter| parameter.to_matcher }
      end

      # @private
      def matches?(available_parameters)
        index = 0
        while (available_parameters.length > 0) && (index < @matchers.length) do
          matcher = @matchers[index]
          return false unless matcher.matches?(available_parameters)
          index += 1
        end
        return true
      end

      # @private
      def mocha_inspect
        "optionally(#{@matchers.map { |matcher| matcher.mocha_inspect }.join(", ") })"
      end

    end

  end

end
