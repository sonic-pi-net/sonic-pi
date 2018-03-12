# -*- coding: utf-8 -*- #

module Rouge
  module Formatters
    # A formatter which renders nothing.
    class Null < Formatter
      tag 'null'

      def initialize(*)
      end

      def stream(tokens, &b)
        tokens.to_a
      end
    end
  end
end
