require 'mocha/parameter_matchers/all_of'
require 'mocha/parameter_matchers/base'

module Mocha

  module ParameterMatchers

    # Matches any object that responds with +true+ to +include?(item)+
    # for all items.
    #
    # @param [*Array] items expected items.
    # @return [Includes] parameter matcher.
    #
    # @see Expectation#with
    #
    # @example Actual parameter includes all items.
    #   object = mock()
    #   object.expects(:method_1).with(includes('foo', 'bar'))
    #   object.method_1(['foo', 'bar', 'baz'])
    #   # no error raised
    #
    # @example Actual parameter does not include all items.
    #   object.method_1(['foo', 'baz'])
    #   # error raised, because ['foo', 'baz'] does not include 'bar'.
    def includes(*items)
      Includes.new(*items)
    end

    # Parameter matcher which matches when actual parameter includes expected values.
    class Includes < Base

      # @private
      def initialize(*items)
        @items = items
      end

      # @private
      def matches?(available_parameters)
        parameter = available_parameters.shift
        return false unless parameter.respond_to?(:include?)

        if @items.size == 1
          return parameter.include?(@items.first)
        else
          includes_matchers = @items.map { |item| Includes.new(item) }
          AllOf.new(*includes_matchers).matches?([parameter])
        end
      end

      # @private
      def mocha_inspect
        item_descriptions = @items.map(&:mocha_inspect)
        "includes(#{item_descriptions.join(', ')})"
      end

    end

  end

end
