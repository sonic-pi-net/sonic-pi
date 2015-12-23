require 'mocha/parameter_matchers/base'

module Mocha

  module ParameterMatchers

    # Matches any +Object+ that is a kind of +klass+.
    #
    # @param [Class] klass expected class.
    # @return [KindOf] parameter matcher.
    #
    # @see Expectation#with
    # @see Kernel#kind_of?
    #
    # @example Actual parameter is a kind of +Integer+.
    #   object = mock()
    #   object.expects(:method_1).with(kind_of(Integer))
    #   object.method_1(99)
    #   # no error raised
    #
    # @example Actual parameter is not a kind of +Integer+.
    #   object = mock()
    #   object.expects(:method_1).with(kind_of(Integer))
    #   object.method_1('string')
    #   # error raised, because method_1 was not called with a kind of Integer
    def kind_of(klass)
      KindOf.new(klass)
    end

    # Parameter matcher which matches when actual parameter is a kind of specified class.
    class KindOf < Base

      # @private
      def initialize(klass)
        @klass = klass
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        parameter.kind_of?(@klass)
      end

      # @private
      def mocha_inspect
        "kind_of(#{@klass.mocha_inspect})"
      end

    end

  end

end
