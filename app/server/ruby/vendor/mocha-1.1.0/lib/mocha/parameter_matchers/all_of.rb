require 'mocha/parameter_matchers/base'

module Mocha

  module ParameterMatchers

    # Matches if all +matchers+ match.
    #
    # @param [*Array<Base>] parameter_matchers parameter matchers.
    # @return [AllOf] parameter matcher.
    #
    # @see Expectation#with
    #
    # @example All parameter matchers match.
    #   object = mock()
    #   object.expects(:method_1).with(all_of(includes(1), includes(3)))
    #   object.method_1([1, 3])
    #   # no error raised
    #
    # @example One of the parameter matchers does not match.
    #   object = mock()
    #   object.expects(:method_1).with(all_of(includes(1), includes(3)))
    #   object.method_1([1, 2])
    #   # error raised, because method_1 was not called with object including 1 and 3
    def all_of(*matchers)
      AllOf.new(*matchers)
    end

    # Parameter matcher which combines a number of other matchers using a logical AND.
    class AllOf < Base

      # @private
      def initialize(*matchers)
        @matchers = matchers
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        @matchers.all? { |matcher| matcher.to_matcher.matches?([parameter]) }
      end

      # @private
      def mocha_inspect
        "all_of(#{@matchers.map { |matcher| matcher.mocha_inspect }.join(", ") })"
      end

    end

  end

end
