module Parser
  module Source

    ##
    # @api private
    #
    class Rewriter::Action
      attr_reader :range, :replacement

      def initialize(range, replacement='')
        @range, @replacement = range, replacement

        freeze
      end

      def to_s
        if @range.length == 0 && @replacement.empty?
          'do nothing'
        elsif @range.length == 0
          "insert #{@replacement.inspect}"
        elsif @replacement.empty?
          "remove #{@range.length} character(s)"
        else
          "replace #{@range.length} character(s) with #{@replacement.inspect}"
        end
      end
    end

  end
end
