# encoding: binary

module Parser

  class Lexer::Literal
    DELIMITERS = { '(' => ')', '[' => ']', '{' => '}', '<' => '>' }
    MONOLITHIC = { :tSTRING_BEG => :tSTRING }

    TYPES = {
    # type      start token     interpolate?
      "'"   => [ :tSTRING_BEG,   false ],
      "<<'" => [ :tSTRING_BEG,   false ],
      '%q'  => [ :tSTRING_BEG,   false ],
      '"'   => [ :tSTRING_BEG,   true  ],
      '<<"' => [ :tSTRING_BEG,   true  ],
      '%'   => [ :tSTRING_BEG,   true  ],
      '%Q'  => [ :tSTRING_BEG,   true  ],

      '%w'  => [ :tQWORDS_BEG,   false ],
      '%W'  => [ :tWORDS_BEG,    true  ],

      '%i'  => [ :tQSYMBOLS_BEG, false ],
      '%I'  => [ :tSYMBOLS_BEG,  true  ],

      ":'"  => [ :tSYMBEG,       false ],
      '%s'  => [ :tSYMBEG,       false ],
      ':"'  => [ :tSYMBEG,       true  ],

      '/'   => [ :tREGEXP_BEG,   true  ],
      '%r'  => [ :tREGEXP_BEG,   true  ],

      '%x'  => [ :tXSTRING_BEG,  true  ],
      '`'   => [ :tXSTRING_BEG,  true  ],
      '<<`' => [ :tXSTRING_BEG,  true  ],
    }

    attr_reader   :heredoc_e, :str_s
    attr_accessor :saved_herebody_s

    def initialize(lexer, str_type, delimiter, str_s, heredoc_e = nil, indent = false)
      @lexer       = lexer
      @nesting     = 1

      # DELIMITERS and TYPES are hashes with keys encoded in binary.
      # Coerce incoming data to the same encoding.
      str_type     = coerce_encoding(str_type)
      delimiter    = coerce_encoding(delimiter)

      unless TYPES.include?(str_type)
        lexer.send(:diagnostic, :error, :unexpected_percent_str,
                   { :type => str_type }, @lexer.send(:range, str_s, str_s + 2))
      end

      # String type. For :'foo', it is :'
      @str_type    = str_type
      # Start of the string type specifier.
      @str_s       = str_s

      @start_tok, @interpolate = TYPES[str_type]
      @start_delim = DELIMITERS.include?(delimiter) ? delimiter : nil
      @end_delim   = DELIMITERS.fetch(delimiter, delimiter)

      @heredoc_e   = heredoc_e
      @indent      = indent

      @interp_braces = 0

      @space_emitted = true

      # Monolithic strings are glued into a single token, e.g.
      # tSTRING_BEG tSTRING_CONTENT tSTRING_END -> tSTRING.
      @monolithic  = (@start_tok == :tSTRING_BEG  &&
                      %w(' ").include?(str_type) &&
                      !heredoc?)

      # Capture opening delimiter in percent-literals.
      unless @heredoc_e || @str_type.end_with?(delimiter)
        @str_type << delimiter
      end

      clear_buffer

      emit_start_tok unless @monolithic
    end

    def interpolate?
      @interpolate
    end

    def words?
      type == :tWORDS_BEG || type == :tQWORDS_BEG ||
        type == :tSYMBOLS_BEG || type == :tQSYMBOLS_BEG
    end

    def regexp?
      type == :tREGEXP_BEG
    end

    def heredoc?
      !!@heredoc_e
    end

    def type
      @start_tok
    end

    def munge_escape?(character)
      character = coerce_encoding(character)

      if words? && character =~ /[ \t\v\r\f\n]/
        true
      else
        ['\\', @start_delim, @end_delim].include?(character)
      end
    end

    def nest_and_try_closing(delimiter, ts, te, lookahead=nil)
      delimiter = coerce_encoding(delimiter)

      if @start_delim && @start_delim == delimiter
        @nesting += 1
      elsif delimiter?(delimiter)
        @nesting -= 1
      end

      # Finalize if last matching delimiter is closed.
      if @nesting == 0
        if words?
          extend_space(ts, ts)
        end

        if lookahead && lookahead[0] == ?: && lookahead[1] != ?: &&
           %w(' ").include?(delimiter) && @start_tok == :tSTRING_BEG
          # This is a quoted label.
          flush_string
          emit(:tLABEL_END, @end_delim, ts, te + 1)
        elsif @monolithic
          # Emit the string as a single token.
          emit(MONOLITHIC[@start_tok], @buffer, @str_s, te)
        else
          # If this is a heredoc, @buffer contains the sentinel now.
          # Just throw it out. Lexer flushes the heredoc after each
          # non-heredoc-terminating \n anyway, so no data will be lost.
          flush_string unless heredoc?

          emit(:tSTRING_END, @end_delim, ts, te)
        end
      end
    end

    def start_interp_brace
      @interp_braces += 1
    end

    def end_interp_brace_and_try_closing
      @interp_braces -= 1

      (@interp_braces == 0)
    end

    def extend_string(string, ts, te)
      if @buffer_s.nil?
        @buffer_s = ts
      end

      @buffer_e = te

      @buffer << string
    end

    def flush_string
      if @monolithic
        emit_start_tok
        @monolithic = false
      end

      unless @buffer.empty?
        emit(:tSTRING_CONTENT, @buffer, @buffer_s, @buffer_e)

        clear_buffer
        extend_content
      end
    end

    def extend_content
      @space_emitted = false
    end

    def extend_space(ts, te)
      flush_string

      unless @space_emitted
        emit(:tSPACE, nil, ts, te)

        @space_emitted = true
      end
    end

    protected

    def delimiter?(delimiter)
      if @indent
        @end_delim == delimiter.lstrip
      else
        @end_delim == delimiter
      end
    end

    def coerce_encoding(string)
      if defined?(Encoding)
        string.dup.force_encoding(Encoding::BINARY)
      else
        string
      end
    end

    def clear_buffer
      @buffer = ''

      # Prime the buffer with lexer encoding; otherwise,
      # concatenation will produce varying results.
      if defined?(Encoding)
        @buffer.force_encoding(@lexer.encoding)
      end

      @buffer_s = nil
      @buffer_e = nil
    end

    def emit_start_tok
      str_e = @heredoc_e || @str_s + @str_type.length
      emit(@start_tok, @str_type, @str_s, str_e)
    end

    def emit(token, type, s, e)
      @lexer.send(:emit, token, type, s, e)
    end
  end

end
