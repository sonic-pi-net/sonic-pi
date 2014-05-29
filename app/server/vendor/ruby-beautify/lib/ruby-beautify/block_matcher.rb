module RBeautify

  class BlockMatcher

    attr_reader :language, :name, :starts, :ends, :options

    def initialize(language, name, starts, ends, options = {})
      @language = language
      @name = name
      @starts = starts
      @ends = ends.nil? ? starts : ends
      @options = options
    end

    class << self
      def parse(language, original_block, line_number, string, current_offset)
        block_end = original_block && original_block.parse_block_end(string, current_offset)

        if (block_start = first_block_start(language,
                                            original_block,
                                            line_number,
                                            string,
                                            current_offset,
                                            block_end.nil? ? nil : block_end.offset))

          block_end = nil
        end

        if block_end
          # Check whether the section of the line which is a block end is also a block start
          if block_end.end_can_also_be_start? &&
              (block_start_candidate = first_block_start(language, block_end.block_start.parent, line_number, string, current_offset)) &&
              block_start_candidate.offset == block_end.offset
            block_start = block_start_candidate
          end

        end

        if block_start
          if debug
            puts "MATCH: '#{string.slice(0, string.length - block_start.match.length - block_start.after_match.length)}<START type=#{block_start.name}>#{block_start.match}</START>#{block_start.after_match}'"
          end
          parse(language, block_start, line_number, block_start.after_match, block_start.end_offset)
        elsif block_end
          if debug
            puts "MATCH: '#{string.slice(0, string.length - block_end.match.length - block_end.after_match.length)}<END>#{block_end.match}</END>#{block_end.after_match}'"
          end
          parse(language, block_end.block_start.parent, line_number, block_end.after_match, block_end.end_offset)
        else
          original_block
        end
      end

      def debug=(value)
        @debug = value
      end

      def debug
        @debug
      end

      private
        def first_block_start(language, parent_block, line_number, string, offset, maximum_offset = nil)
          first_block_start = nil
          language.matchers.each do |matcher|
            if matcher.can_nest?(parent_block)
              if (block_start_candidate = matcher.parse_block_start(string, parent_block, offset, line_number)) &&
                  (maximum_offset.nil? || maximum_offset > block_start_candidate.offset)
                first_block_start = block_start_candidate
                maximum_offset = first_block_start.offset
              end
            end
          end
          first_block_start
        end
    end

    def parse_block_start(string, parent_block, offset, line_number)
      if !string.empty? && (match = starts.match(string))
        RBeautify::BlockStart.new(self, parent_block, line_number, offset + match.begin(0), match[0], match.post_match)
      end
    end

    def indent_end_line?(block)
      evaluate_option_for_block(:indent_end_line, block)
    end

    def indent_size(block)
      evaluate_option_for_block(:indent_size, block) || language.indent_size
    end

    # Look for blocks within the content of this one
    def parse_content?
      options[:parse_content] != false
    end

    # Indent the content of this block
    def format_content?
      options[:format_content] != false
    end

    def can_nest?(parent_block)
      return false unless parent_block.nil? || parent_block.parse_content?

      if options[:nest_only]
        parent_block && options[:nest_only].include?(parent_block.name)
      else
        parent_block.nil? ||
          options[:nest_except].nil? || !options[:nest_except].include?(parent_block.name)
      end
    end

    def ends?
      ends != false
    end

    def end_is_implicit?
      options[:end] == :implicit
    end

    def end_can_also_be_start?
      if ends == starts
        options[:end_can_also_be_start] == true
      else
        options[:end_can_also_be_start] != false
      end
    end

    def negate_ends_match?
      options[:negate_ends_match]
    end

    # True if blocks can contain the escape character \ which needs to be
    # checked for on end match
    def escape_character?
      options[:escape_character] == true
    end

    def inspect
      name
    end

    private
      def evaluate_option_for_block(key, block)
        if options[key] && options[key].respond_to?(:call)
          options[key].call(block)
        else
          options[key]
        end
      end

  end
end
