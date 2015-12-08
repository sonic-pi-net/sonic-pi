require 'mocha/parameter_matchers/base'

module Mocha

  module ParameterMatchers

    # Matches any parameters. This is used as the default for a newly built expectation.
    #
    # @return [AnyParameters] parameter matcher.
    #
    # @see Expectation#with
    #
    # @example Any parameters will match.
    #   object = mock()
    #   object.expects(:method_1).with(any_parameters)
    #   object.method_1(1, 2, 3, 4)
    #   # no error raised
    #
    #   object = mock()
    #   object.expects(:method_1).with(any_parameters)
    #   object.method_1(5, 6, 7, 8, 9, 0)
    #   # no error raised
    def any_parameters
      AnyParameters.new
    end

    # Parameter matcher which always matches whatever the parameters.
    class AnyParameters < Base

      # @private
      def matches?(available_parameters)
        while available_parameters.length > 0 do
          available_parameters.shift
        end
        return true
      end

      # @private
      def mocha_inspect
        "any_parameters"
      end

    end

  end

end
