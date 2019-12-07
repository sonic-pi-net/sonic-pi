module RBeautify

  class BlockStart

    attr_reader :block_matcher, :parent, :offset, :match, :after_match, :line_number

    class << self
      def first_common_ancestor(first, second)
        if first.nil? || second.nil?
          nil
        else
          (first.ancestors & second.ancestors).last
        end
      end
    end

    def initialize(block_matcher, parent, line_number, offset, match, after_match)
      @block_matcher = block_matcher
      @parent = parent
      @offset = offset
      @match = match
      @after_match = after_match
      @line_number = line_number
    end

    def end_offset
      offset + match.length
    end

    def parse_block_end(string, offset)
      block_end = parse_explicit_block_end(string, offset)

      # Handle case where end is implicit
      if block_end.nil? && end_is_implicit? && parent
        block_end = parent.parse_block_end(string, offset)
      end

      block_end
    end

    def format_content?
      block_matcher.format_content?
    end

    def parse_content?
      block_matcher.parse_content?
    end

    def indent_end_line?
      block_matcher.indent_end_line?(self)
    end

    def total_indent_size
      parent.nil? ? indent_size : parent.total_indent_size + indent_size
    end

    def indent_size
      block_matcher.indent_size(self)
    end

    def end_is_implicit?
      block_matcher.end_is_implicit?
    end

    def name
      block_matcher.name
    end

    # Returns true if strict ancestor of
    def strict_ancestor_of?(block_start)
      block_start && block_start.parent && (self == block_start.parent || strict_ancestor_of?(block_start.parent))
    end

    def ancestors
      if parent
        parent.ancestors + [self]
      else
        [self]
      end
    end

    private
      def ends?
        block_matcher.ends?
      end

      def negate_ends_match?
        block_matcher.negate_ends_match?
      end

      def escape_character?
        block_matcher.escape_character?
      end

      def parse_explicit_block_end(string, offset)
        block_end = nil

        if ends?

          if match = block_matcher.ends.match(string)
            unless negate_ends_match?
              if escape_character? &&
                  ((escape_chars = match.pre_match.match(/\\*$/)) && (escape_chars[0].size % 2 == 1))
                # If there are an odd number of escape characters just before
                # the match then this match should be skipped
                return parse_explicit_block_end(match.post_match, offset + escape_chars[0].size + match[0].length)
              else
                return RBeautify::BlockEnd.new(self, offset + match.begin(0), match[0], match.post_match)
              end
            end
          elsif negate_ends_match?
            return RBeautify::BlockEnd.new(self, offset, '', string)
          end

        end
      end

  end
end
