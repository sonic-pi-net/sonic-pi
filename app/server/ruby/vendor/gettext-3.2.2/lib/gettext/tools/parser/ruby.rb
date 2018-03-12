# -*- coding: utf-8 -*-
=begin
  parser/ruby.rb - parser for ruby script

  Copyright (C) 2013       Kouhei Sutou <kou@clear-code.com>
  Copyright (C) 2003-2009  Masao Mutoh
  Copyright (C) 2005       speakillof
  Copyright (C) 2001,2002  Yasushi Shoji, Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby or LGPL.

=end

require "irb/ruby-lex"
require "stringio"
require "gettext/po_entry"

module GetText
  class RubyLexX < RubyLex  # :nodoc: all
    # Parser#parse resemlbes RubyLex#lex
    def parse
      until (  (tk = token).kind_of?(RubyToken::TkEND_OF_SCRIPT) && !@continue or tk.nil?  )
        s = get_readed
        if RubyToken::TkSTRING === tk or RubyToken::TkDSTRING === tk
          def tk.value
            @value
          end

          def tk.value=(s)
            @value = s
          end

          if @here_header
            s = s.sub(/\A.*?\n/, "").sub(/^.*\n\Z/, "")
          else
            begin
              s = eval(s)
            rescue Exception
              # Do nothing.
            end
          end

          tk.value = s
        end

        if $DEBUG
          if tk.is_a? TkSTRING or tk.is_a? TkDSTRING
            $stderr.puts("#{tk}: #{tk.value}")
          elsif tk.is_a? TkIDENTIFIER
            $stderr.puts("#{tk}: #{tk.name}")
          else
            $stderr.puts(tk)
          end
        end

        yield tk
      end
      return nil
    end

    # Original parser does not keep the content of the comments,
    # so monkey patching this with new token type and extended
    # identify_comment implementation
    RubyToken.def_token :TkCOMMENT_WITH_CONTENT, TkVal

    def identify_comment
      @ltype = "#"
      get_readed # skip the hash sign itself

      while ch = getc
        if ch == "\n"
          @ltype = nil
          ungetc
          break
        end
      end
      return Token(TkCOMMENT_WITH_CONTENT, get_readed)
    end

  end

  # Extends POEntry for RubyParser.
  # Implements a sort of state machine to assist the parser.
  module POEntryForRubyParser
    # Supports parsing by setting attributes by and by.
    def set_current_attribute(str)
      param = @param_type[@param_number]
      raise ParseError, "no more string parameters expected" unless param
      set_value(param, str)
    end

    def init_param
      @param_number = 0
      self
    end

    def advance_to_next_attribute
      @param_number += 1
    end
  end
  class POEntry
    include POEntryForRubyParser
    alias :initialize_old :initialize
    def initialize(type)
      initialize_old(type)
      init_param
    end
  end

  class RubyParser
    ID = ["gettext", "_", "N_", "sgettext", "s_"]
    PLURAL_ID = ["ngettext", "n_", "Nn_", "ns_", "nsgettext"]
    MSGCTXT_ID = ["pgettext", "p_"]
    MSGCTXT_PLURAL_ID = ["npgettext", "np_"]

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
      po = []
      file = StringIO.new(source)
      rl = RubyLexX.new
      rl.set_input(file)
      rl.skip_space = true
      #rl.readed_auto_clean_up = true

      po_entry = nil
      line_no = nil
      last_comment = ""
      reset_comment = false
      ignore_next_comma = false
      rl.parse do |tk|
        begin
          ignore_current_comma = ignore_next_comma
          ignore_next_comma = false
          case tk
          when RubyToken::TkIDENTIFIER, RubyToken::TkCONSTANT
            if store_po_entry(po, po_entry, line_no, last_comment)
              last_comment = ""
            end
            if ID.include?(tk.name)
              po_entry = POEntry.new(:normal)
            elsif PLURAL_ID.include?(tk.name)
              po_entry = POEntry.new(:plural)
            elsif MSGCTXT_ID.include?(tk.name)
              po_entry = POEntry.new(:msgctxt)
            elsif MSGCTXT_PLURAL_ID.include?(tk.name)
              po_entry = POEntry.new(:msgctxt_plural)
            else
              po_entry = nil
            end
            line_no = tk.line_no.to_s
          when RubyToken::TkSTRING, RubyToken::TkDSTRING
            po_entry.set_current_attribute tk.value if po_entry
          when RubyToken::TkPLUS, RubyToken::TkNL
            #do nothing
          when RubyToken::TkINTEGER
            ignore_next_comma = true
          when RubyToken::TkCOMMA
            unless ignore_current_comma
              po_entry.advance_to_next_attribute if po_entry
            end
          else
            if store_po_entry(po, po_entry, line_no, last_comment)
              po_entry = nil
              last_comment = ""
            end
          end
        rescue
          $stderr.print "\n\nError"
          $stderr.print " parsing #{@path}:#{tk.line_no}\n\t #{source.lines.to_a[tk.line_no - 1]}" if tk
          $stderr.print "\n #{$!.inspect} in\n"
          $stderr.print $!.backtrace.join("\n")
          $stderr.print "\n"
          exit 1
        end

        case tk
        when RubyToken::TkCOMMENT_WITH_CONTENT
          last_comment = "" if reset_comment
          if last_comment.empty?
            comment1 = tk.value.lstrip
            if comment_to_be_extracted?(comment1)
              last_comment << comment1
            end
          else
            last_comment += "\n"
            last_comment += tk.value
          end
          reset_comment = false
        when RubyToken::TkNL
        else
          reset_comment = true
        end
      end
      po
    end

    private
    def store_po_entry(po, po_entry, line_no, last_comment) #:nodoc:
      if po_entry && po_entry.msgid
        po_entry.references << @path + ":" + line_no
        po_entry.add_comment(last_comment) unless last_comment.empty?
        po << po_entry
        true
      else
        false
      end
    end

    def comment_to_be_extracted?(comment)
      return false unless @options.has_key?(:comment_tag)

      tag = @options[:comment_tag]
      return true if tag.nil?

      /\A#{Regexp.escape(tag)}/ === comment
    end
  end
end
