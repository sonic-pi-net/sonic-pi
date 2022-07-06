# Copyright (C) 2012-2019  Sutou Kouhei <kou@clear-code.com>
# Copyright (C) 2010  masone (Christian Felder) <ema@rh-productions.ch>
# Copyright (C) 2009  Masao Mutoh
#
# License: Ruby's or LGPL
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "gettext/po_format"

module GetText
  class ParseError < StandardError
  end

  # Contains data related to the expression or sentence that
  # is to be translated.
  class POEntry
    class InvalidTypeError < StandardError
    end

    class NoMsgidError < StandardError
    end

    class NoMsgctxtError < StandardError
    end

    class NoMsgidPluralError < StandardError
    end

    PARAMS = {
      :normal => [:msgid, :separator, :msgstr],
      :plural => [:msgid, :msgid_plural, :separator, :msgstr],
      :msgctxt => [:msgctxt, :msgid, :msgstr],
      :msgctxt_plural => [:msgctxt, :msgid, :msgid_plural, :msgstr]
    }

    # Required
    attr_reader :type          # :normal, :plural, :msgctxt, :msgctxt_plural
    attr_accessor :msgid
    attr_accessor :msgstr
    # Options
    attr_accessor :msgid_plural
    attr_accessor :separator
    attr_accessor :msgctxt
    attr_accessor :references    # ["file1:line1", "file2:line2", ...]
    attr_accessor :translator_comment
    attr_accessor :extracted_comment
    # @return [Array<String>] The flags for this PO entry.
    # @since 3.0.4
    attr_accessor :flags
    attr_accessor :previous
    attr_accessor :comment

    # Create the object. +type+ should be :normal, :plural, :msgctxt or :msgctxt_plural.
    def initialize(type)
      self.type = type
      @translator_comment = nil
      @extracted_comment = nil
      @references = []
      @flags = []
      @previous = nil
      @msgctxt = nil
      @msgid = nil
      @msgid_plural = nil
      @msgstr = nil
    end

    # Support for extracted comments. Explanation s.
    # http://www.gnu.org/software/gettext/manual/gettext.html#Names
    # @return [void]
    def add_comment(new_comment)
      if (new_comment and ! new_comment.empty?)
        @extracted_comment ||= String.new
        @extracted_comment << "\n" unless @extracted_comment.empty?
        @extracted_comment << new_comment
      end
    end

    # @return [String, nil] The flag of the PO entry.
    # @deprecated Since 3.0.4. Use {#flags} instead.
    def flag
      @flags.first
    end

    # Set the new flag for the PO entry.
    #
    # @param [String, nil] flag The new flag.
    # @deprecated Since 3.0.4. Use {#flags=} instead.
    def flag=(flag)
      if flag.nil?
        @flags = []
      else
        @flags = [flag]
      end
    end

    # Checks if the self has same attributes as other.
    def ==(other)
      not other.nil? and
        type == other.type and
        msgid == other.msgid and
        msgstr == other.msgstr and
        msgid_plural == other.msgid_plural and
        separator == other.separator and
        msgctxt == other.msgctxt and
        translator_comment == other.translator_comment and
        extracted_comment == other.extracted_comment and
        references == other.references and
        flags == other.flags and
        previous == other.previous and
        comment == other.comment
    end

    def type=(type)
      unless PARAMS.has_key?(type)
        raise(InvalidTypeError, "\"%s\" is invalid type." % type)
      end
      @type = type
      @param_type = PARAMS[@type]
    end

    # Checks if the other translation target is mergeable with
    # the current one. Relevant are msgid and translation context (msgctxt).
    def mergeable?(other)
      other && other.msgid == self.msgid && other.msgctxt == self.msgctxt
    end

    # Merges two translation targets with the same msgid and returns the merged
    # result. If one is declared as plural and the other not, then the one
    # with the plural wins.
    def merge(other)
      return self unless other
      unless mergeable?(other)
        message = "Translation targets do not match: \n" +
          "  self: #{self.inspect}\n  other: '#{other.inspect}'"
        raise ParseError, message
      end
      if other.msgid_plural && !msgid_plural
        res = other
        unless res.references.include?(references[0])
          res.references += references
          res.add_comment(extracted_comment)
        end
      else
        res = self
        unless res.references.include?(other.references[0])
          res.references += other.references
          res.add_comment(other.extracted_comment)
        end
      end
      res
    end

    # Format the po entry in PO format.
    #
    # @param [Hash] options
    # @option options (see Formatter#initialize)
    def to_s(options={})
      raise(NoMsgidError, "msgid is nil.") unless @msgid

      formatter = Formatter.new(self, options)
      formatter.format
    end

    # Returns true if the type is kind of msgctxt.
    def msgctxt?
      [:msgctxt, :msgctxt_plural].include?(@type)
    end

    # Returns true if the type is kind of plural.
    def plural?
      [:plural, :msgctxt_plural].include?(@type)
    end

    # @return true if the entry is header entry, false otherwise.
    #   Header entry is normal type and has empty msgid.
    def header?
      @type == :normal and @msgid == ""
    end

    # @return true if the entry is obsolete entry, false otherwise.
    #   Obsolete entry is normal type and has :last msgid.
    def obsolete?
      @type == :normal and @msgid == :last
    end

    # @return true if the entry is fuzzy entry, false otherwise.
    #   Fuzzy entry has "fuzzy" flag.
    def fuzzy?
      @flags.include?("fuzzy")
    end

    # @return true if the entry is translated entry, false otherwise.
    def translated?
      return false if fuzzy?
      return false if @msgstr.nil? or @msgstr.empty?
      true
    end

    def [](number_or_param)
      __send__(resolve_param(number_or_param))
    end

    def []=(number_or_param, value)
      __send__("#{resolve_param(number_or_param)}=", value)
    end

    private
    def resolve_param(number_or_param)
      case number_or_param
      when Integer
        param = @param_type[number_or_param]
        raise ParseError, 'no more string parameters expected' unless param
        param
      else
        number_or_param
      end
    end

    # sets or extends the value of a translation target params like msgid,
    # msgctxt etc.
    #   param is symbol with the name of param
    #   value - new value
    def set_value(param, value)
      send "#{param}=", (send(param) || '') + value
    end

    class Formatter
      class << self
        def escape(string)
          return "" if string.nil?

          string.gsub(/([\\"\t\n])/) do
            special_character = $1
            case special_character
            when "\t"
              "\\t"
            when "\n"
              "\\n"
            else
              "\\#{special_character}"
            end
          end
        end
      end

      include POFormat

      DEFAULT_MAX_LINE_WIDTH = 78

      # @param [POEntry] entry The entry to be formatted.
      # @param [Hash] options
      # @option options [Bool] :include_translator_comment (true)
      #   Includes translator comments in formatted string if true.
      # @option options [Bool] :include_extracted_comment (true)
      #   Includes extracted comments in formatted string if true.
      # @option options [Bool] :include_reference_comment (true)
      #   Includes reference comments in formatted string if true.
      # @option options [Bool] :include_flag_comment (true)
      #   Includes flag comments in formatted string if true.
      # @option options [Bool] :include_previous_comment (true)
      #   Includes previous comments in formatted string if true.
      # @option options [Bool] :include_all_comments (true)
      #   Includes all comments in formatted string if true.
      #   Other specific `:include_XXX` options get preference over
      #   this option.
      #   You can remove all comments by specifying this option as
      #   false and omitting other `:include_XXX` options.
      # @option options [Integer] :max_line_width (78)
      #   Wraps long lines that is longer than the `:max_line_width`.
      #   Don't break long lines if `:max_line_width` is less than 0
      #   such as `-1`.
      # @option options [Encoding] :encoding (nil)
      #   Encodes to the specific encoding.
      def initialize(entry, options={})
        @entry = entry
        @options = normalize_options(options)
      end

      def format
        if @entry.obsolete?
          return format_obsolete_comment(@entry.comment)
        end

        str = format_comments

        # msgctxt, msgid, msgstr
        if @entry.msgctxt?
          if @entry.msgctxt.nil?
            no_msgctxt_message = "This POEntry is a kind of msgctxt " +
                                   "but the msgctxt property is nil. " +
                                   "msgid: #{@entry.msgid}"
            raise(NoMsgctxtError, no_msgctxt_message)
          end
          str << "msgctxt " << format_message(@entry.msgctxt)
        end

        str << "msgid " << format_message(@entry.msgid)
        if @entry.plural?
          if @entry.msgid_plural.nil?
            no_plural_message = "This POEntry is a kind of plural " +
                                  "but the msgid_plural property is nil. " +
                                  "msgid: #{@entry.msgid}"
            raise(NoMsgidPluralError, no_plural_message)
          end

          str << "msgid_plural " << format_message(@entry.msgid_plural)

          if @entry.msgstr.nil?
            str << "msgstr[0] \"\"\n"
            str << "msgstr[1] \"\"\n"
          else
            msgstrs = @entry.msgstr.split("\000", -1)
            msgstrs.each_with_index do |msgstr, index|
              str << "msgstr[#{index}] " << format_message(msgstr)
            end
          end
        else
          str << "msgstr "
          str << format_message(@entry.msgstr)
        end

        encode(str)
      end

      private
      def normalize_options(options)
        options = options.dup
        include_comment_keys = [
          :include_translator_comment,
          :include_extracted_comment,
          :include_reference_comment,
          :include_flag_comment,
          :include_previous_comment,
        ]
        if options[:include_all_comments].nil?
          options[:include_all_comments] = true
        end
        default_include_comment_value = options[:include_all_comments]
        include_comment_keys.each do |key|
          options[key] = default_include_comment_value if options[key].nil?
        end
        options[:max_line_width] ||= DEFAULT_MAX_LINE_WIDTH
        options
      end

      def include_translator_comment?
        @options[:include_translator_comment]
      end

      def include_extracted_comment?
        @options[:include_extracted_comment]
      end

      def include_reference_comment?
        @options[:include_reference_comment]
      end

      def include_flag_comment?
        @options[:include_flag_comment]
      end

      def include_previous_comment?
        @options[:include_previous_comment]
      end

      def format_comments
        formatted_comment = String.new
        if include_translator_comment?
          formatted_comment << format_translator_comment
        end
        if include_extracted_comment?
          formatted_comment << format_extracted_comment
        end
        if include_reference_comment?
          formatted_comment << format_reference_comment
        end
        if include_flag_comment?
          formatted_comment << format_flag_comment
        end
        if include_previous_comment?
          formatted_comment << format_previous_comment
        end
        formatted_comment
      end

      def format_translator_comment
        format_comment("#", @entry.translator_comment)
      end

      def format_extracted_comment
        format_comment(EXTRACTED_COMMENT_MARK, @entry.extracted_comment)
      end

      def format_reference_comment
        max_line_width = @options[:max_line_width]
        formatted_reference = String.new
        if not @entry.references.nil? and not @entry.references.empty?
          formatted_reference << REFERENCE_COMMENT_MARK
          line_width = 2
          @entry.references.each do |reference|
            if max_line_width > 0 and
                line_width + reference.size > max_line_width
              formatted_reference << "\n"
              formatted_reference <<  "#{REFERENCE_COMMENT_MARK} #{reference}"
              line_width = 3 + reference.size
            else
              formatted_reference << " #{reference}"
              line_width += 1 + reference.size
            end
          end

          formatted_reference << "\n"
        end
        formatted_reference
      end

      def format_flag_comment
        formatted_flags = String.new
        @entry.flags.each do |flag|
          formatted_flags << format_comment(FLAG_MARK, flag)
        end
        formatted_flags
      end

      def format_previous_comment
        format_comment(PREVIOUS_COMMENT_MARK, @entry.previous)
      end

      def format_comment(mark, comment)
        return "" if comment.nil?

        formatted_comment = String.new
        comment.each_line do |comment_line|
          if comment_line == "\n"
            formatted_comment << "#{mark}\n"
          else
            formatted_comment << "#{mark} #{comment_line.strip}\n"
          end
        end
        formatted_comment
      end

      def format_obsolete_comment(comment)
        mark = "#~"
        return "" if comment.nil?

        formatted_comment = String.new
        comment.each_line do |comment_line|
          if /\A#[^~]/ =~ comment_line or comment_line.start_with?(mark)
            formatted_comment << "#{comment_line.chomp}\n"
          elsif comment_line == "\n"
            formatted_comment << "\n"
          else
            formatted_comment << "#{mark} #{comment_line.strip}\n"
          end
        end
        formatted_comment
      end

      def format_message(message)
        empty_formatted_message = "\"\"\n"
        return empty_formatted_message if message.nil?

        chunks = wrap_message(message)
        return empty_formatted_message if chunks.empty?

        formatted_message = String.new
        if chunks.size > 1 or chunks.first.end_with?("\n")
          formatted_message << empty_formatted_message
        end
        chunks.each do |chunk|
          formatted_message << "\"#{escape(chunk)}\"\n"
        end
        formatted_message
      end

      def escape(string)
        self.class.escape(string)
      end

      def wrap_message(message)
        return [message] if message.empty?

        max_line_width = @options[:max_line_width]

        chunks = []
        message.each_line do |line|
          if max_line_width <= 0
            chunks << line
          else
            # TODO: use character width instead of the number of characters
            line.scan(/.{1,#{max_line_width}}/m) do |chunk|
              chunks << chunk
            end
          end
        end
        chunks
      end

      def encode(string)
        encoding = @options[:encoding]
        return string if encoding.nil?
        string.encode(encoding)
      end
    end
  end
end
