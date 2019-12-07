require 'mocha/parameter_matchers/base'
require 'uri'

module Mocha
  module ParameterMatchers

    # Matches a URI without regard to the ordering of parameters in the query string.
    #
    # @param [String] uri URI to match.
    # @return [QueryStringMatches] parameter matcher.
    #
    # @see Expectation#with
    #
    # @example Actual URI has equivalent query string.
    #   object = mock()
    #   object.expects(:method_1).with(has_equivalent_query_string('http://example.com/foo?a=1&b=2))
    #   object.method_1('http://example.com/foo?b=2&a=1')
    #   # no error raised
    #
    # @example Actual URI does not have equivalent query string.
    #   object = mock()
    #   object.expects(:method_1).with(has_equivalent_query_string('http://example.com/foo?a=1&b=2))
    #   object.method_1('http://example.com/foo?a=1&b=3')
    #   # error raised, because the query parameters were different
    def has_equivalent_query_string(uri)
      QueryStringMatches.new(uri)
    end

    # Parameter matcher which matches URIs with equivalent query strings.
    class QueryStringMatches < Base

      # @private
      def initialize(uri)
        @uri = URI.parse(uri)
      end

      # @private
      def matches?(available_parameters)
        actual = explode(URI.parse(available_parameters.shift))
        expected = explode(@uri)
        actual == expected
      end

      # @private
      def mocha_inspect
        "has_equivalent_query_string(#{@uri.mocha_inspect})"
      end

    private
      # @private
      def explode(uri)
        query_hash = (uri.query || '').split('&').inject({}){ |h, kv| h.merge(Hash[*kv.split('=')]) }
        URI::Generic::COMPONENT.inject({}){ |h, k| h.merge(k => uri.__send__(k)) }.merge(:query => query_hash)
      end

    end
  end
end
