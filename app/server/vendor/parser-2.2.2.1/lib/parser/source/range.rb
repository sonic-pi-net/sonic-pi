module Parser
  module Source

    ##
    # A range of characters in a particular source buffer.
    #
    # The range is always exclusive, i.e. a range with `begin_pos` of 3 and
    # `end_pos` of 5 will contain the following characters:
    #
    #     example
    #        ^^
    #
    # @!attribute [r] source_buffer
    #  @return [Parser::Diagnostic::Engine]
    #
    # @!attribute [r] begin_pos
    #  @return [Integer] index of the first character in the range
    #
    # @!attribute [r] end_pos
    #  @return [Integer] index of the character after the last character in the range
    #
    # @api public
    #
    class Range
      attr_reader :source_buffer
      attr_reader :begin_pos, :end_pos

      ##
      # @param [Buffer]  source_buffer
      # @param [Integer] begin_pos
      # @param [Integer] end_pos
      #
      def initialize(source_buffer, begin_pos, end_pos)
        @source_buffer       = source_buffer
        @begin_pos, @end_pos = begin_pos, end_pos

        freeze
      end

      ##
      # @return [Range] a zero-length range located just before the beginning
      #   of this range.
      #
      def begin
        Range.new(@source_buffer, @begin_pos, @begin_pos)
      end

      ##
      # @return [Range] a zero-length range located just after the end
      #   of this range.
      #
      def end
        Range.new(@source_buffer, @end_pos, @end_pos)
      end

      ##
      # @return [Integer] amount of characters included in this range.
      #
      def size
        @end_pos - @begin_pos
      end

      alias length size

      ##
      # Line number of the beginning of this range. By default, the first line
      # of a buffer is 1; as such, line numbers are most commonly one-based.
      #
      # @see Buffer
      # @return [Integer] line number of the beginning of this range.
      #
      def line
        line, _ = @source_buffer.decompose_position(@begin_pos)

        line
      end

      ##
      # @return [Integer] zero-based column number of the beginning of this range.
      #
      def column
        _, column = @source_buffer.decompose_position(@begin_pos)

        column
      end

      ##
      # @return [::Range] a range of columns spanned by this range.
      # @raise RangeError
      #
      def column_range
        if self.begin.line != self.end.line
          raise RangeError, "#{self.inspect} spans more than one line"
        end

        self.begin.column...self.end.column
      end

      ##
      # @return [String] a line of source code containing the beginning of this range.
      #
      def source_line
        @source_buffer.source_line(line)
      end

      ##
      # @return [String] all source code covered by this range.
      #
      def source
        @source_buffer.source[self.begin_pos...self.end_pos]
      end

      ##
      # `is?` provides a concise way to compare the source corresponding to this range.
      # For example, `r.source == '(' || r.source == 'begin'` is equivalent to
      # `r.is?('(', 'begin')`.
      #
      def is?(*what)
        what.include?(source)
      end

      ##
      # @return [Array(Integer)] a set of character indexes contained in this range.
      #
      def to_a
        (@begin_pos...@end_pos).to_a
      end

      ##
      # Composes a GNU/Clang-style string representation of the beginning of this
      # range.
      #
      # For example, for the following range in file `foo.rb`,
      #
      #     def foo
      #         ^^^
      #
      # `to_s` will return `foo.rb:1:5`.
      # Note that the column index is one-based.
      #
      # @return [String]
      #
      def to_s
        line, column = @source_buffer.decompose_position(@begin_pos)

        [@source_buffer.name, line, column + 1].join(':')
      end

      ##
      # @param [Integer] new_size
      # @return [Range] a range beginning at the same point as this range and length `new_size`.
      #
      def resize(new_size)
        Range.new(@source_buffer, @begin_pos, @begin_pos + new_size)
      end

      ##
      # @param [Range] other
      # @return [Range] smallest possible range spanning both this range and `other`.
      #
      def join(other)
        Range.new(@source_buffer,
            [@begin_pos, other.begin_pos].min,
            [@end_pos,   other.end_pos].max)
      end

      ##
      # Compares ranges.
      # @return [Boolean]
      #
      def ==(other)
        other.is_a?(Range) &&
          @source_buffer == other.source_buffer &&
          @begin_pos     == other.begin_pos     &&
          @end_pos       == other.end_pos
      end

      ##
      # @return [String] a human-readable representation of this range.
      #
      def inspect
        "#<Parser::Source::Range #{@source_buffer.name} #{@begin_pos}...#{@end_pos}>"
      end
    end

  end
end
