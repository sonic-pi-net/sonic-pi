=begin
  parser/ruby.rb - parser for ruby script

  Copyright (C) 2013-2021  Sutou Kouhei <kou@clear-code.com>
  Copyright (C) 2003-2009  Masao Mutoh
  Copyright (C) 2005       speakillof
  Copyright (C) 2001,2002  Yasushi Shoji, Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby or LGPL.

=end

require "ripper"
require "stringio"

require "gettext/po_entry"

module GetText
  class RubyParser
    class POExtractor < Ripper::Filter
      ID = ["gettext", "_", "N_", "sgettext", "s_"]
      PLURAL_ID = ["ngettext", "n_", "Nn_", "ns_", "nsgettext"]
      MSGCTXT_ID = ["pgettext", "p_"]
      MSGCTXT_PLURAL_ID = ["npgettext", "np_"]

      attr_accessor :use_comment
      attr_accessor :comment_tag
      def initialize(*args)
        super(*args)
        @start_block = false
        @in_block_arguments = false
        @ignore_next_comma = false
        @need_definition_name = false
        @current_po_entry = nil
        @current_po_entry_nth_attribute = 0
        @use_comment = false
        @comment_tag = nil
        @last_comment = ""
        @reset_comment = false
        @string_mark_stack = []
        @string_stack = []
      end

      def process_on_op(token, po)
        if @start_block
          @in_block_arguments = (token == "|")
        else
          if @in_block_arguments and token == "|"
            @in_block_arguments = false
          end
        end
        po
      end

      def process_on_kw(token, po)
        store_po_entry(po)
        case token
        when "do"
          @start_block = true
        end
        po
      end

      def process_on_lbrace(token, po)
        store_po_entry(po)
        @start_block = (state == Ripper::EXPR_BEG)
        po
      end

      def process_on_ident(token, po)
        store_po_entry(po)

        return po if @in_block_arguments
        return po if state.allbits?(Ripper::EXPR_ENDFN)

        case token
        when *ID
          @current_po_entry = POEntry.new(:normal)
        when *PLURAL_ID
          @current_po_entry = POEntry.new(:plural)
        when *MSGCTXT_ID
          @current_po_entry = POEntry.new(:msgctxt)
        when *MSGCTXT_PLURAL_ID
          @current_po_entry = POEntry.new(:msgctxt_plural)
        end
        if @current_po_entry
          @current_po_entry.add_comment(@last_comment) unless @last_comment.empty?
          @last_comment = ""
          @current_po_entry.references << "#{filename}:#{lineno}"
          @current_po_entry_nth_attribute = 0
        end
        po
      end

      def process_on_const(token, po)
        case token
        when "N_", "Nn_"
          # TODO: Check the next token is :on_lparen
          process_on_ident(token, po)
        else
          po
        end
      end

      def process_on_comment(token, po)
        @last_comment = "" if @reset_comment
        @reset_comment = false
        if @last_comment.empty?
          content = token.gsub(/\A#\s*/, "").chomp
          if comment_to_be_extracted?(content)
            @last_comment << content
          end
        else
          content = token.gsub(/\A#/, "").chomp
          @last_comment << "\n"
          @last_comment << content
        end
        po
      end

      def process_on_sp(token, po)
        po
      end

      def process_on_tstring_beg(token, po)
        if token.start_with?("%Q")
          @string_mark_stack << "\""
        elsif token.start_with?("%q")
          @string_mark_stack << "'"
        elsif token.start_with?("%")
          @string_mark_stack << "\""
        else
          @string_mark_stack << token
        end
        @string_stack << ""
        po
      end

      def process_on_tstring_content(token, po)
        case @string_mark_stack.last
        when "\"", "`"
          @string_stack.last << token.gsub(/\\./) do |data|
            case data
            when "\\n"
              "\n"
            when "\\t"
              "\t"
            when "\\\\"
              "\\"
            when "\\\""
              "\""
            when "\\\#"
              "#"
            else
              data
            end
          end
        else
          @string_stack.last << token.gsub(/\\./) do |data|
            case data
            when "\\\\"
              "\\"
            when "\\'"
              "'"
            else
              data
            end
          end
        end
        po
      end

      def process_on_tstring_end(token, po)
        @ignore_next_comma = false
        string_mark = @string_mark_stack.pop
        case string_mark
        when "\"", "'"
          last_string = @string_stack.pop
          if @current_po_entry and last_string
            @current_po_entry[@current_po_entry_nth_attribute] =
              (@current_po_entry[@current_po_entry_nth_attribute] || "") +
              last_string
          end
        end
        po
      end

      def process_on_heredoc_beg(token, po)
        if token.end_with?("'")
          @string_mark_stack << "'"
        else
          @string_mark_stack << "\""
        end
        @string_stack << ""
        po
      end

      def process_on_heredoc_end(token, po)
        process_on_tstring_end(token, po)
      end

      def process_on_regexp_beg(token, po)
        @string_mark_stack << "\""
        @string_stack << ""
        po
      end

      def process_on_regexp_end(token, po)
        @string_mark_stack.pop
        @string_stack.pop
        po
      end

      def process_on_embexpr_beg(token, po)
        @current_po_entry = nil
        @current_po_entry_nth_attribute = 0
        po
      end

      def process_on_int(token, po)
        @ignore_next_comma = true
        po
      end

      def process_on_comma(token, po)
        unless @ignore_next_comma
          if @current_po_entry
            @current_po_entry_nth_attribute += 1
          end
        end
        po
      end

      def process_on_rparen(token, po)
        store_po_entry(po)
        po
      end

      def process_on_nl(token, po)
        @reset_comment = true
        po
      end

      def process_on_symbeg(token, po)
        if token.start_with?("%s") or [":'", ":\""].include?(token)
          @string_mark_stack << ":"
          @string_stack << ""
        end
        po
      end

      def process_on_backtick(token, po)
        @string_mark_stack << "`"
        @string_stack << ""
        po
      end

      def process_on_symbols_beg(token, po)
        @string_mark_stack << "\""
        @string_stack << ""
        po
      end

      def process_on_qsymbols_beg(token, po)
        @string_mark_stack << token
        @string_stack << ""
        po
      end

      def process_on_words_beg(token, po)
        @string_mark_stack << "\""
        @string_stack << ""
        po
      end

      def process_on_qwords_beg(token, po)
        @string_mark_stack << token
        @string_stack << ""
        po
      end

      def on_default(event, token, po)
        trace(event, token) do
          process_method = "process_#{event}"
          start_block = @start_block
          if respond_to?(process_method)
            po = __send__(process_method, token, po)
          end
          if start_block and event != :on_sp
            @start_block = false
          end
          po
        end
      end

      private
      @@debug = ENV["GETTEXT_RUBY_PARSER_DEBUG"]
      def debug?
        @@debug
      end

      def trace(event_name, token)
        if debug?
          status = [
            event_name,
            token,
            state,
          ]
          status << :start_block if @start_block
          status << :in_block_arguments if @in_block_arguments
          pp status
        end
        yield
      end

      def store_po_entry(po)
        return if @current_po_entry.nil?
        po << @current_po_entry if @current_po_entry.msgid
        @current_po_entry = nil
        @current_po_entry_nth_attribute = 0
      end

      def comment_to_be_extracted?(comment)
        return false unless @use_comment

        return true if @comment_tag.nil?

        comment.start_with?(@comment_tag)
      end
    end

    class << self
      def target?(file)  # :nodoc:
        true # always true, as the default parser.
      end

      # Parses Ruby script located at `path`.
      #
      # This is a short cut method. It equals to `new(path,
      # options).parse`.
      #
      # @param (see #initialize)
      # @option (see #initialize)
      # @return (see #parse)
      # @see #initialize
      # @see #parse
      def parse(path, options={})
        parser = new(path, options)
        parser.parse
      end
    end

    #
    # @example `:comment_tag` option: String tag
    #   path = "hello.rb"
    #   # content:
    #   #   # TRANSLATORS: This is a comment to translators.
    #   #   _("Hello")
    #   #
    #   #   # This is a comment for programmers.
    #   #   # TRANSLATORS: This is a comment to translators.
    #   #   # This is also a comment to translators.
    #   #   _("World")
    #   #
    #   #   # This is a comment for programmers.
    #   #   # This is also a comment for programmers
    #   #   # because all lines don't start with "TRANSRATORS:".
    #   #   _("Bye")
    #   options = {:comment_tag => "TRANSLATORS:"}
    #   parser = GetText::RubyParser.new(path, options)
    #   parser.parse
    #   # => [
    #   #   POEntry<
    #   #     :msgid => "Hello",
    #   #     :extracted_comment =>
    #   #       "TRANSLATORS: This is a comment to translators.",
    #   #   >,
    #   #   POEntry<
    #   #     :msgid => "World",
    #   #     :extracted_comment =>
    #   #       "TRANSLATORS: This is a comment to translators.\n" +
    #   #       "This is also a comment to translators.",
    #   #   >,
    #   #   POEntry<
    #   #     :msgid => "Bye",
    #   #     :extracted_comment => nil,
    #   #   >,
    #   # ]
    #
    # @example `:comment_tag` option: nil tag
    #   path = "hello.rb"
    #   # content:
    #   #   # This is a comment to translators.
    #   #   # This is also a comment for translators.
    #   #   _("Hello")
    #   options = {:comment_tag => nil}
    #   parser = GetText::RubyParser.new(path, options)
    #   parser.parse
    #   # => [
    #   #   POEntry<
    #   #     :msgid => "Hello",
    #   #     :extracted_comment =>
    #   #       "This is a comment to translators.\n" +
    #   #       " This is also a comment for translators.",
    #   #   >,
    #   # ]
    #
    # @param path [String] Ruby script path to be parsed
    # @param options [Hash] Options
    # @option options [String, nil] :comment_tag The tag to
    #   detect comments to be extracted. The extracted comments are
    #   used to deliver messages to translators from programmers.
    #
    #   If the tag is String and a line in a comment start with the
    #   tag, the line and the following lines are extracted.
    #
    #   If the tag is nil, all comments are extracted.
    def initialize(path, options={})
      @path = path
      @options = options
    end

    # Extracts messages from @path.
    #
    # @return [Array<POEntry>] Extracted messages
    def parse
      source = IO.read(@path)

      encoding = detect_encoding(source) || source.encoding
      source.force_encoding(encoding)

      parse_source(source)
    end

    def detect_encoding(source)
      binary_source = source.dup.force_encoding("ASCII-8BIT")
      if /\A.*coding\s*[=:]\s*([[:alnum:]\-_]+)/ =~ binary_source
        $1.gsub(/-(?:unix|mac|dos)\z/, "")
      else
        nil
      end
    end

    def parse_source(source)
      extractor = POExtractor.new(source, @path)
      if @options.key?(:comment_tag)
        extractor.use_comment = true
        extractor.comment_tag = @options[:comment_tag]
      end
      extractor.parse([])
    end
  end
end
