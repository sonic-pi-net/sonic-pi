%%machine lex; # % fix highlighting

#
# === BEFORE YOU START ===
#
# Read the Ruby Hacking Guide chapter 11, available in English at
# http://whitequark.org/blog/2013/04/01/ruby-hacking-guide-ch-11-finite-state-lexer/
#
# Remember two things about Ragel scanners:
#
#   1) Longest match wins.
#
#   2) If two matches have the same length, the first
#      in source code wins.
#
# General rules of making Ragel and Bison happy:
#
#  * `p` (position) and `@te` contain the index of the character
#    they're pointing to ("current"), plus one. `@ts` contains the index
#    of the corresponding character. The code for extracting matched token is:
#
#       @source[@ts...@te]
#
#  * If your input is `foooooooobar` and the rule is:
#
#       'f' 'o'+
#
#    the result will be:
#
#       foooooooobar
#       ^ ts=0   ^ p=te=9
#
#  * A Ragel lexer action should not emit more than one token, unless
#    you know what you are doing.
#
#  * All Ragel commands (fnext, fgoto, ...) end with a semicolon.
#
#  * If an action emits the token and transitions to another state, use
#    these Ragel commands:
#
#       emit($whatever)
#       fnext $next_state; fbreak;
#
#    If you perform `fgoto` in an action which does not emit a token nor
#    rewinds the stream pointer, the parser's side-effectful,
#    context-sensitive lookahead actions will break in a hard to detect
#    and debug way.
#
#  * If an action does not emit a token:
#
#       fgoto $next_state;
#
#  * If an action features lookbehind, i.e. matches characters with the
#    intent of passing them to another action:
#
#       p = @ts - 1
#       fgoto $next_state;
#
#    or, if the lookbehind consists of a single character:
#
#       fhold; fgoto $next_state;
#
#  * Ragel merges actions. So, if you have `e_lparen = '(' %act` and
#    `c_lparen = '('` and a lexer action `e_lparen | c_lparen`, the result
#    _will_ invoke the action `act`.
#
#    e_something stands for "something with **e**mbedded action".
#
#  * EOF is explicit and is matched by `c_eof`. If you want to introspect
#    the state of the lexer, add this rule to the state:
#
#       c_eof => do_eof;
#
#  * If you proceed past EOF, the lexer will complain:
#
#       NoMethodError: undefined method `ord' for nil:NilClass
#

class Parser::Lexer

  %% write data nofinal;
  # %

  ESCAPES = {
    'a' => "\a", 'b'  => "\b", 'e'  => "\e", 'f' => "\f",
    'n' => "\n", 'r'  => "\r", 's'  => "\s", 't' => "\t",
    'v' => "\v", '\\' => "\\"
  }

  attr_reader   :source_buffer
  attr_reader   :encoding

  attr_accessor :diagnostics
  attr_accessor :static_env
  attr_accessor :force_utf32

  attr_accessor :cond, :cmdarg, :in_kwarg

  attr_accessor :tokens, :comments

  def initialize(version)
    @version    = version
    @static_env = nil

    @tokens     = nil
    @comments   = nil

    reset
  end

  def reset(reset_state=true)
    # Ragel state:
    if reset_state
      # Unit tests set state prior to resetting lexer.
      @cs     = self.class.lex_en_line_begin

      @cond   = StackState.new('cond')
      @cmdarg = StackState.new('cmdarg')
      @cond_stack   = []
      @cmdarg_stack = []
    end

    @force_utf32   = false # Set to true by some tests

    @source        = nil # source string
    @source_pts    = nil # @source as a codepoint array
    @encoding      = nil # target encoding for output strings

    @p             = 0   # stream position (saved manually in #advance)
    @ts            = nil # token start
    @te            = nil # token end
    @act           = 0   # next action

    @stack         = []  # state stack
    @top           = 0   # state stack top pointer

    # Lexer state:
    @token_queue   = []
    @literal_stack = []

    @eq_begin_s    = nil # location of last encountered =begin
    @sharp_s       = nil # location of last encountered #

    @newline_s     = nil # location of last encountered newline

    @num_base      = nil # last numeric base
    @num_digits_s  = nil # starting position of numeric digits
    @num_suffix_s  = nil # starting position of numeric suffix
    @num_xfrm      = nil # numeric suffix-induced transformation

    @escape_s      = nil # starting position of current sequence
    @escape        = nil # last escaped sequence, as string

    # See below the section on parsing heredocs.
    @heredoc_e     = nil
    @herebody_s    = nil

    # Ruby 1.9 ->() lambdas emit a distinct token if do/{ is
    # encountered after a matching closing parenthesis.
    @paren_nest    = 0
    @lambda_stack  = []

    # If the lexer is in `command state' (aka expr_value)
    # at the entry to #advance, it will transition to expr_cmdarg
    # instead of expr_arg at certain points.
    @command_state = false

    # True at the end of "def foo a:"
    @in_kwarg      = false
  end

  def source_buffer=(source_buffer)
    @source_buffer = source_buffer

    if @source_buffer
      @source = @source_buffer.source

      if defined?(Encoding)
        @encoding   = @source.encoding

        # This is a workaround for 1.9.2, which (without force_encoding)
        # would convert the result to UTF-8 (source encoding of lexer.rl).
        @source    += "\0".force_encoding(@encoding)
      else
        @source    += "\0"
      end

      if defined?(Encoding) && @source.encoding == Encoding::UTF_8
        @source_pts = @source.unpack('U*')
      else
        @source_pts = @source.unpack('C*')
      end

      if (@source_pts.size > 1_000_000 && @source.respond_to?(:encode)) ||
         @force_utf32
        # A heuristic: if the buffer is larger than 1M, then
        # store it in UTF-32 and convert the tokens as they're
        # going out. If it's smaller, the conversion overhead
        # dominates runtime and this stops being beneficial.
        #
        # This is not really a good heuristic, as the result
        # heavily depends on token/character ratio. If it's low,
        # say the gem consists mostly of long identifiers and
        # symbols, then storing the source in UTF-8 would be faster.
        #
        # Patches accepted.
        @source = @source.encode(Encoding::UTF_32LE)
      end

      if @source_pts[0] == 0xfeff
        # Skip byte order mark.
        @p = 1
      end
    else
      @source     = nil
      @source_pts = nil
    end
  end

  LEX_STATES = {
    :line_begin    => lex_en_line_begin,
    :expr_dot      => lex_en_expr_dot,
    :expr_fname    => lex_en_expr_fname,
    :expr_value    => lex_en_expr_value,
    :expr_beg      => lex_en_expr_beg,
    :expr_mid      => lex_en_expr_mid,
    :expr_arg      => lex_en_expr_arg,
    :expr_cmdarg   => lex_en_expr_cmdarg,
    :expr_end      => lex_en_expr_end,
    :expr_endarg   => lex_en_expr_endarg,
    :expr_endfn    => lex_en_expr_endfn,
    :expr_labelarg => lex_en_expr_labelarg,

    :interp_string => lex_en_interp_string,
    :interp_words  => lex_en_interp_words,
    :plain_string  => lex_en_plain_string,
    :plain_words   => lex_en_plain_string,
  }

  def state
    LEX_STATES.invert.fetch(@cs, @cs)
  end

  def state=(state)
    @cs = LEX_STATES.fetch(state)
  end

  def push_cmdarg
    @cmdarg_stack.push(@cmdarg)
    @cmdarg = StackState.new("cmdarg.#{@cmdarg_stack.count}")
  end

  def pop_cmdarg
    @cmdarg = @cmdarg_stack.pop
  end

  def push_cond
    @cond_stack.push(@cond)
    @cond = StackState.new("cond.#{@cond_stack.count}")
  end

  def pop_cond
    @cond = @cond_stack.pop
  end

  # Return next token: [type, value].
  def advance
    if @token_queue.any?
      return @token_queue.shift
    end

    # Ugly, but dependent on Ragel output. Consider refactoring it somehow.
    _lex_trans_keys         = self.class.send :_lex_trans_keys
    _lex_key_spans          = self.class.send :_lex_key_spans
    _lex_index_offsets      = self.class.send :_lex_index_offsets
    _lex_indicies           = self.class.send :_lex_indicies
    _lex_trans_targs        = self.class.send :_lex_trans_targs
    _lex_trans_actions      = self.class.send :_lex_trans_actions
    _lex_to_state_actions   = self.class.send :_lex_to_state_actions
    _lex_from_state_actions = self.class.send :_lex_from_state_actions
    _lex_eof_trans          = self.class.send :_lex_eof_trans

    p, pe, eof = @p, @source.length + 1, @source.length + 1

    @command_state = (@cs == self.class.lex_en_expr_value ||
                      @cs == self.class.lex_en_line_begin)

    %% write exec;
    # %

    @p = p

    if @token_queue.any?
      @token_queue.shift
    elsif @cs == self.class.lex_error
      [ false, [ '$error', range(p - 1, p) ] ]
    else
      [ false, [ '$eof',   range(p, p)     ] ]
    end
  end

  protected

  def eof_codepoint?(point)
    [0x04, 0x1a, 0x00].include? point
  end

  def version?(*versions)
    versions.include?(@version)
  end

  def stack_pop
    @top -= 1
    @stack[@top]
  end

  if "".respond_to?(:encode)
    def encode_escape(ord)
      ord.chr.force_encoding(@encoding)
    end

    def tok(s = @ts, e = @te)
      @source[s...e].encode(@encoding)
    end
  else
    def encode_escape(ord)
      ord.chr
    end

    def tok(s = @ts, e = @te)
      @source[s...e]
    end
  end

  def range(s = @ts, e = @te)
    Parser::Source::Range.new(@source_buffer, s, e)
  end

  def emit(type, value = tok, s = @ts, e = @te)
    token = [ type, [ value, range(s, e) ] ]

    @token_queue.push(token)

    @tokens.push(token) if @tokens

    token
  end

  def emit_table(table, s = @ts, e = @te)
    value = tok(s, e)

    emit(table[value], value, s, e)
  end

  def emit_do(do_block=false)
    if @cond.active?
      emit(:kDO_COND)
    elsif @cmdarg.active? || do_block
      emit(:kDO_BLOCK)
    else
      emit(:kDO)
    end
  end

  def arg_or_cmdarg
    if @command_state
      self.class.lex_en_expr_cmdarg
    else
      self.class.lex_en_expr_arg
    end
  end

  def emit_comment(s = @ts, e = @te)
    if @comments
      @comments.push(Parser::Source::Comment.new(range(s, e)))
    end

    if @tokens
      @tokens.push([ :tCOMMENT, [ tok(s, e), range(s, e) ] ])
    end

    nil
  end

  def diagnostic(type, reason, arguments=nil, location=range, highlights=[])
    @diagnostics.process(
        Parser::Diagnostic.new(type, reason, arguments, location, highlights))
  end

  #
  # === LITERAL STACK ===
  #

  def push_literal(*args)
    new_literal = Literal.new(self, *args)
    @literal_stack.push(new_literal)

    if new_literal.words?
      if new_literal.interpolate?
        self.class.lex_en_interp_words
      else
        self.class.lex_en_plain_words
      end
    else
      if new_literal.interpolate?
        self.class.lex_en_interp_string
      else
        self.class.lex_en_plain_string
      end
    end
  end

  def literal
    @literal_stack.last
  end

  def pop_literal
    old_literal = @literal_stack.pop

    if old_literal.type == :tREGEXP_BEG
      # Fetch modifiers.
      self.class.lex_en_regexp_modifiers
    else
      self.class.lex_en_expr_end
    end
  end

  # Mapping of strings to parser tokens.

  PUNCTUATION = {
    '='   => :tEQL,     '&'   => :tAMPER2,  '|'   => :tPIPE,
    '!'   => :tBANG,    '^'   => :tCARET,   '+'   => :tPLUS,
    '-'   => :tMINUS,   '*'   => :tSTAR2,   '/'   => :tDIVIDE,
    '%'   => :tPERCENT, '~'   => :tTILDE,   ','   => :tCOMMA,
    ';'   => :tSEMI,    '.'   => :tDOT,     '..'  => :tDOT2,
    '...' => :tDOT3,    '['   => :tLBRACK2, ']'   => :tRBRACK,
    '('   => :tLPAREN2, ')'   => :tRPAREN,  '?'   => :tEH,
    ':'   => :tCOLON,   '&&'  => :tANDOP,   '||'  => :tOROP,
    '-@'  => :tUMINUS,  '+@'  => :tUPLUS,   '~@'  => :tTILDE,
    '**'  => :tPOW,     '->'  => :tLAMBDA,  '=~'  => :tMATCH,
    '!~'  => :tNMATCH,  '=='  => :tEQ,      '!='  => :tNEQ,
    '>'   => :tGT,      '>>'  => :tRSHFT,   '>='  => :tGEQ,
    '<'   => :tLT,      '<<'  => :tLSHFT,   '<='  => :tLEQ,
    '=>'  => :tASSOC,   '::'  => :tCOLON2,  '===' => :tEQQ,
    '<=>' => :tCMP,     '[]'  => :tAREF,    '[]=' => :tASET,
    '{'   => :tLCURLY,  '}'   => :tRCURLY,  '`'   => :tBACK_REF2,
    '!@'  => :tBANG,
  }

  PUNCTUATION_BEGIN = {
    '&'   => :tAMPER,   '*'   => :tSTAR,    '**'  => :tDSTAR,
    '+'   => :tUPLUS,   '-'   => :tUMINUS,  '::'  => :tCOLON3,
    '('   => :tLPAREN,  '{'   => :tLBRACE,  '['   => :tLBRACK,
  }

  KEYWORDS = {
    'if'     => :kIF_MOD,      'unless'   => :kUNLESS_MOD,
    'while'  => :kWHILE_MOD,   'until'    => :kUNTIL_MOD,
    'rescue' => :kRESCUE_MOD,  'defined?' => :kDEFINED,
    'BEGIN'  => :klBEGIN,      'END'      => :klEND,
  }

  KEYWORDS_BEGIN = {
    'if'     => :kIF,          'unless'   => :kUNLESS,
    'while'  => :kWHILE,       'until'    => :kUNTIL,
    'rescue' => :kRESCUE,      'defined?' => :kDEFINED,
  }

  %w(class module def undef begin end then elsif else ensure case when
     for break next redo retry in do return yield super self nil true
     false and or not alias __FILE__ __LINE__ __ENCODING__).each do |keyword|
    KEYWORDS_BEGIN[keyword] = KEYWORDS[keyword] = :"k#{keyword.upcase}"
  end

  %%{
  # %

  access @;
  getkey (@source_pts[p] || 0);

  # === CHARACTER CLASSES ===
  #
  # Pay close attention to the differences between c_any and any.
  # c_any does not include EOF and so will cause incorrect behavior
  # for machine subtraction (any-except rules) and default transitions
  # for scanners.

  action do_nl {
    # Record position of a newline for precise location reporting on tNL
    # tokens.
    #
    # This action is embedded directly into c_nl, as it is idempotent and
    # there are no cases when we need to skip it.
    @newline_s = p
  }

  c_nl       = '\n' $ do_nl;
  c_space    = [ \t\r\f\v];
  c_space_nl = c_space | c_nl;

  c_eof      = 0x04 | 0x1a | 0 | zlen; # ^D, ^Z, \0, EOF
  c_eol      = c_nl | c_eof;
  c_any      = any - c_eof;

  c_nl_zlen  = c_nl | zlen;
  c_line     = any - c_nl_zlen;

  c_unicode  = c_any - 0x00..0x7f;
  c_upper    = [A-Z];
  c_lower    = [a-z_]  | c_unicode;
  c_alpha    = c_lower | c_upper;
  c_alnum    = c_alpha | [0-9];

  action do_eof {
    # Sit at EOF indefinitely. #advance would return $eof each time.
    # This allows to feed the lexer more data if needed; this is only used
    # in tests.
    #
    # Note that this action is not embedded into e_eof like e_heredoc_nl and e_bs
    # below. This is due to the fact that scanner state at EOF is observed
    # by tests, and encapsulating it in a rule would break the introspection.
    fhold; fbreak;
  }

  #
  # === TOKEN DEFINITIONS ===
  #

  # All operators are punctuation. There is more to punctuation
  # than just operators. Operators can be overridden by user;
  # punctuation can not.

  # A list of operators which are valid in the function name context, but
  # have different semantics in others.
  operator_fname      = '[]' | '[]=' | '`'  | '-@' | '+@' | '~@'  | '!@' ;

  # A list of operators which can occur within an assignment shortcut (+ → +=).
  operator_arithmetic = '&'  | '|'   | '&&' | '||' | '^'  | '+'   | '-'  |
                        '*'  | '/'   | '**' | '~'  | '<<' | '>>'  | '%'  ;

  # A list of all user-definable operators not covered by groups above.
  operator_rest       = '=~' | '!~' | '==' | '!=' | '!'   | '===' |
                        '<'  | '<=' | '>'  | '>=' | '<=>' | '=>'  ;

  # Note that `{` and `}` need to be referred to as e_lbrace and e_rbrace,
  # as they are ambiguous with interpolation `#{}` and should be counted.
  # These braces are not present in punctuation lists.

  # A list of punctuation which has different meaning when used at the
  # beginning of expression.
  punctuation_begin   = '-'  | '+'  | '::' | '('  | '['  |
                        '*'  | '**' | '&'  ;

  # A list of all punctuation except punctuation_begin.
  punctuation_end     = ','  | '='  | '->' | '('  | '['  | ']'   |
                        '::' | '?'  | ':'  | '.'  | '..' | '...' ;

  # A list of keywords which have different meaning at the beginning of expression.
  keyword_modifier    = 'if'     | 'unless' | 'while'  | 'until' | 'rescue' ;

  # A list of keywords which accept an argument-like expression, i.e. have the
  # same post-processing as method calls or commands. Example: `yield 1`,
  # `yield (1)`, `yield(1)`, are interpreted as if `yield` was a function.
  keyword_with_arg    = 'yield'  | 'super'  | 'not'    | 'defined?' ;

  # A list of keywords which accept a literal function name as an argument.
  keyword_with_fname  = 'def'    | 'undef'  | 'alias'  ;

  # A list of keywords which accept an expression after them.
  keyword_with_value  = 'else'   | 'case'   | 'ensure' | 'module' | 'elsif' | 'then'  |
                        'for'    | 'in'     | 'do'     | 'when'   | 'begin' | 'class' |
                        'and'    | 'or'     ;

  # A list of keywords which accept a value, and treat the keywords from
  # `keyword_modifier` list as modifiers.
  keyword_with_mid    = 'rescue' | 'return' | 'break'  | 'next'   ;

  # A list of keywords which do not accept an expression after them.
  keyword_with_end    = 'end'    | 'self'   | 'true'   | 'false'  | 'retry'    |
                        'redo'   | 'nil'    | 'BEGIN'  | 'END'    | '__FILE__' |
                        '__LINE__' | '__ENCODING__';

  # All keywords.
  keyword             = keyword_with_value | keyword_with_mid |
                        keyword_with_end   | keyword_with_arg |
                        keyword_with_fname | keyword_modifier ;

  constant       = c_upper c_alnum*;
  bareword       = c_alpha c_alnum*;

  call_or_var    = c_lower c_alnum*;
  class_var      = '@@' bareword;
  instance_var   = '@' bareword;
  global_var     = '$'
      ( bareword | digit+
      | [`'+~*$&?!@/\\;,.=:<>"] # `
      | '-' c_alnum
      )
  ;

  # Ruby accepts (and fails on) variables with leading digit
  # in literal context, but not in unquoted symbol body.
  class_var_v    = '@@' c_alnum+;
  instance_var_v = '@' c_alnum+;

  label          = bareword [?!]? ':';

  #
  # === NUMERIC PARSING ===
  #

  int_hex  = ( xdigit+ '_' )* xdigit* '_'? ;
  int_dec  = ( digit+ '_' )* digit* '_'? ;
  int_bin  = ( [01]+ '_' )* [01]* '_'? ;

  flo_int  = [1-9] [0-9]* ( '_' digit+ )* | '0';
  flo_frac = '.' ( digit+ '_' )* digit+;
  flo_pow  = [eE] [+\-]? ( digit+ '_' )* digit+;

  int_suffix =
    ''   % { @num_xfrm = lambda { |chars|  emit(:tINTEGER,   chars) } }
  | 'r'  % { @num_xfrm = lambda { |chars|  emit(:tRATIONAL,  Rational(chars)) } }
  | 'i'  % { @num_xfrm = lambda { |chars|  emit(:tIMAGINARY, Complex(0, chars)) } }
  | 'ri' % { @num_xfrm = lambda { |chars|  emit(:tIMAGINARY, Complex(0, Rational(chars))) } };

  flo_pow_suffix =
    ''   % { @num_xfrm = lambda { |chars| emit(:tFLOAT,     Float(chars)) } }
  | 'i'  % { @num_xfrm = lambda { |chars| emit(:tIMAGINARY, Complex(0, Float(chars))) } };

  flo_suffix =
    flo_pow_suffix
  | 'r'  % { @num_xfrm = lambda { |chars| emit(:tRATIONAL,  Rational(chars)) } }
  | 'ri' % { @num_xfrm = lambda { |chars| emit(:tIMAGINARY, Complex(0, Rational(chars))) } };

  #
  # === ESCAPE SEQUENCE PARSING ===
  #

  # Escape parsing code is a Ragel pattern, not a scanner, and therefore
  # it shouldn't directly raise errors or perform other actions with side effects.
  # In reality this would probably just mess up error reporting in pathological
  # cases, through.

  # The amount of code required to parse \M\C stuff correctly is ridiculous.

  escaped_nl = "\\" c_nl;

  action unicode_points {
    @escape = ""

    codepoints  = tok(@escape_s + 2, p - 1)
    codepoint_s = @escape_s + 2

    codepoints.split(/[ \t]/).each do |codepoint_str|
      codepoint = codepoint_str.to_i(16)

      if codepoint >= 0x110000
        @escape = lambda do
          diagnostic :error, :unicode_point_too_large, nil,
                     range(codepoint_s, codepoint_s + codepoint_str.length)
        end

        break
      end

      @escape     += codepoint.chr(Encoding::UTF_8)
      codepoint_s += codepoint_str.length + 1
    end
  }

  action unescape_char {
    char = @source[p - 1].chr
    @escape = ESCAPES.fetch(char, char)
  }

  action invalid_complex_escape {
    @escape = lambda do
      diagnostic :fatal, :invalid_escape
    end
  }

  action slash_c_char {
    @escape = encode_escape(@escape[0].ord & 0x9f)
  }

  action slash_m_char {
    @escape = encode_escape(@escape[0].ord | 0x80)
  }

  maybe_escaped_char = (
        '\\' c_any      %unescape_char
    | ( c_any - [\\] )  % { @escape = @source[p - 1].chr }
  );

  maybe_escaped_ctrl_char = ( # why?!
        '\\' c_any      %unescape_char %slash_c_char
    |   '?'             % { @escape = "\x7f" }
    | ( c_any - [\\?] ) % { @escape = @source[p - 1].chr } %slash_c_char
  );

  escape = (
      # \377
      [0-7]{1,3}
      % { @escape = encode_escape(tok(@escape_s, p).to_i(8) % 0x100) }

      # \xff
    | ( 'x' xdigit{1,2}
        % { @escape = encode_escape(tok(@escape_s + 1, p).to_i(16)) }
      # \u263a
      | 'u' xdigit{4}
        % { @escape = tok(@escape_s + 1, p).to_i(16).chr(Encoding::UTF_8) }
      )

      # %q[\x]
    | 'x' ( c_any - xdigit )
      % {
        @escape = lambda do
          diagnostic :fatal, :invalid_hex_escape, nil,
                     range(@escape_s - 1, p + 2)
        end
      }

      # %q[\u123] %q[\u{12]
    | 'u' ( c_any{0,4}  -
            xdigit{4}   -           # \u1234 is valid
            ( '{' xdigit{1,3}       # \u{1 \u{12 \u{123 are valid
            | '{' xdigit [ \t}] any # \u{1. \u{1} are valid
            | '{' xdigit{2} [ \t}]  # \u{12. \u{12} are valid
            )
          )
      % {
        @escape = lambda do
          diagnostic :fatal, :invalid_unicode_escape, nil,
                     range(@escape_s - 1, p)
        end
      }

      # \u{123 456}
    | 'u{' ( xdigit{1,6} [ \t] )*
      ( xdigit{1,6} '}'
        %unicode_points
      | ( xdigit* ( c_any - xdigit - '}' )+ '}'
        | ( c_any - '}' )* c_eof
        | xdigit{7,}
        ) % {
          @escape = lambda do
            diagnostic :fatal, :unterminated_unicode, nil,
                       range(p - 1, p)
          end
        }
      )

      # \C-\a \cx
    | ( 'C-' | 'c' ) escaped_nl?
      maybe_escaped_ctrl_char

      # \M-a
    | 'M-' escaped_nl?
      maybe_escaped_char
      %slash_m_char

      # \C-\M-f \M-\cf \c\M-f
    | ( ( 'C-'   | 'c' ) escaped_nl?   '\\M-'
      |   'M-\\'         escaped_nl? ( 'C-'   | 'c' ) ) escaped_nl?
      maybe_escaped_ctrl_char
      %slash_m_char

    | 'C' c_any %invalid_complex_escape
    | 'M' c_any %invalid_complex_escape
    | ( 'M-\\C' | 'C-\\M' ) c_any %invalid_complex_escape

    | ( c_any - [0-7xuCMc] ) %unescape_char

    | c_eof % {
      diagnostic :fatal, :escape_eof, nil, range(p - 1, p)
    }
  );

  # Use rules in form of `e_bs escape' when you need to parse a sequence.
  e_bs = '\\' % {
    @escape_s = p
    @escape   = nil
  };

  #
  # === STRING AND HEREDOC PARSING ===
  #

  # Heredoc parsing is quite a complex topic. First, consider that heredocs
  # can be arbitrarily nested. For example:
  #
  #     puts <<CODE
  #     the result is: #{<<RESULT.inspect
  #       i am a heredoc
  #     RESULT
  #     }
  #     CODE
  #
  # which, incidentally, evaluates to:
  #
  #     the result is: "  i am a heredoc\n"
  #
  # To parse them, lexer refers to two kinds (remember, nested heredocs)
  # of positions in the input stream, namely @heredoc_e
  # (HEREDOC declaration End) and @herebody_s (HEREdoc BODY line Start).
  #
  # @heredoc_e is simply contained inside the corresponding Literal, and
  # when the heredoc is closed, the lexing is restarted from that position.
  #
  # @herebody_s is quite more complex. First, @herebody_s changes after each
  # heredoc line is lexed. This way, at '\n' tok(@herebody_s, @te) always
  # contains the current line, and also when a heredoc is started, @herebody_s
  # contains the position from which the heredoc will be lexed.
  #
  # Second, as (insanity) there are nested heredocs, we need to maintain a
  # stack of these positions. Each time #push_literal is called, it saves current
  # @heredoc_s to literal.saved_herebody_s, and after an interpolation (possibly
  # containing another heredocs) is closed, the previous value is restored.

  e_heredoc_nl = c_nl % {
    # After every heredoc was parsed, @herebody_s contains the
    # position of next token after all heredocs.
    if @herebody_s
      p = @herebody_s
      @herebody_s = nil
    end
  };

  action extend_string {
    string = @source[@ts...@te]
    string = string.encode(@encoding) if string.respond_to?(:encode)

    # tLABEL_END is only possible in non-cond context on >= 2.2
    if @version >= 22 && !@cond.active?
      lookahead = @source[@te...@te+2]
      lookahead = lookahead.encode(@encoding) if lookahead.respond_to?(:encode)
    end

    if !literal.heredoc? && (token = literal.nest_and_try_closing(string, @ts, @te, lookahead))
      if token[0] == :tLABEL_END
        p += 1
        pop_literal
        fnext expr_labelarg;
      else
        fnext *pop_literal;
      end
       fbreak;
    else
      literal.extend_string(string, @ts, @te)
    end
  }

  action extend_string_escaped {
    if literal.nest_and_try_closing('\\', @ts, @ts + 1)
      # If the literal is actually closed by the backslash,
      # rewind the input prior to consuming the escape sequence.
      p = @escape_s - 1
      fnext *pop_literal; fbreak;
    else
      # Get the first character after the backslash.
      escaped_char = @source[@escape_s].chr

      if literal.munge_escape? escaped_char
        # If this particular literal uses this character as an opening
        # or closing delimiter, it is an escape sequence for that
        # particular character. Write it without the backslash.

        if literal.regexp? && escaped_char == '\\'
          # Regular expressions should include backslashes in their escaped
          # form.
          literal.extend_string(tok, @ts, @te)
        else
          literal.extend_string(escaped_char, @ts, @te)
        end
      else
        # It does not. So this is an actual escape sequence, yay!
        # Two things to consider here.
        #
        # 1. The `escape' rule should be pure and so won't raise any
        #    errors by itself. Instead, it stores them in lambdas.
        #
        # 2. Non-interpolated literals do not go through the aforementioned
        #    rule. As \\ and \' (and variants) are munged, the full token
        #    should always be written for such literals.

        @escape.call if @escape.respond_to? :call

        if literal.regexp?
          # Regular expressions should include escape sequences in their
          # escaped form. On the other hand, escaped newlines are removed.
          literal.extend_string(tok.gsub("\\\n", ''), @ts, @te)
        else
          literal.extend_string(@escape || tok, @ts, @te)
        end
      end
    end
  }

  # Extend a string with a newline or a EOF character.
  # As heredoc closing line can immediately precede EOF, this action
  # has to handle such case specially.
  action extend_string_eol {
    if @te == pe
      diagnostic :fatal, :string_eof, nil,
                 range(literal.str_s, literal.str_s + 1)
    end

    if literal.heredoc?
      line = tok(@herebody_s, @ts).gsub(/\r+$/, '')

      if version?(18, 19, 20)
        # See ruby:c48b4209c
        line = line.gsub(/\r.*$/, '')
      end

      # Try ending the heredoc with the complete most recently
      # scanned line. @herebody_s always refers to the start of such line.
      if literal.nest_and_try_closing(line, @herebody_s, @ts)
        # Adjust @herebody_s to point to the next line.
        @herebody_s = @te

        # Continue regular lexing after the heredoc reference (<<END).
        p = literal.heredoc_e - 1
        fnext *pop_literal; fbreak;
      else
        # Ditto.
        @herebody_s = @te
      end
    else
      # Try ending the literal with a newline.
      if literal.nest_and_try_closing(tok, @ts, @te)
        fnext *pop_literal; fbreak;
      end

      if @herebody_s
        # This is a regular literal intertwined with a heredoc. Like:
        #
        #     p <<-foo+"1
        #     bar
        #     foo
        #     2"
        #
        # which, incidentally, evaluates to "bar\n1\n2".
        p = @herebody_s - 1
        @herebody_s = nil
      end
    end

    if literal.words? && !eof_codepoint?(@source_pts[p])
      literal.extend_space @ts, @te
    else
      # A literal newline is appended if the heredoc was _not_ closed
      # this time (see fbreak above). See also Literal#nest_and_try_closing
      # for rationale of calling #flush_string here.
      literal.extend_string tok, @ts, @te
      literal.flush_string
    end
  }

  action extend_string_space {
    literal.extend_space @ts, @te
  }

  #
  # === INTERPOLATION PARSING ===
  #

  # Interpolations with immediate variable names simply call into
  # the corresponding machine.

  interp_var = '#' ( global_var | class_var_v | instance_var_v );

  action extend_interp_var {
    literal.flush_string
    literal.extend_content

    emit(:tSTRING_DVAR, nil, @ts, @ts + 1)

    p = @ts
    fcall expr_variable;
  }

  # Interpolations with code blocks must match nested curly braces, as
  # interpolation ending is ambiguous with a block ending. So, every
  # opening and closing brace should be matched with e_[lr]brace rules,
  # which automatically perform the counting.
  #
  # Note that interpolations can themselves be nested, so brace balance
  # is tied to the innermost literal.
  #
  # Also note that literals themselves should not use e_[lr]brace rules
  # when matching their opening and closing delimiters, as the amount of
  # braces inside the characters of a string literal is independent.

  interp_code = '#{';

  e_lbrace = '{' % {
    @cond.push(false); @cmdarg.push(false)

    if literal
      literal.start_interp_brace
    end
  };

  e_rbrace = '}' % {
    if literal
      if literal.end_interp_brace_and_try_closing
        if version?(18, 19)
          emit(:tRCURLY, '}', p - 1, p)
        else
          emit(:tSTRING_DEND, '}', p - 1, p)
        end

        if literal.saved_herebody_s
          @herebody_s = literal.saved_herebody_s
        end

        fhold;
        fnext *stack_pop;
        fbreak;
      end
    end
  };

  action extend_interp_code {
    literal.flush_string
    literal.extend_content

    emit(:tSTRING_DBEG, '#{')

    if literal.heredoc?
      literal.saved_herebody_s = @herebody_s
      @herebody_s = nil
    end

    literal.start_interp_brace
    fcall expr_value;
  }

  # Actual string parsers are simply combined from the primitives defined
  # above.

  interp_words := |*
      interp_code => extend_interp_code;
      interp_var  => extend_interp_var;
      e_bs escape => extend_string_escaped;
      c_space+    => extend_string_space;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  interp_string := |*
      interp_code => extend_interp_code;
      interp_var  => extend_interp_var;
      e_bs escape => extend_string_escaped;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  plain_words := |*
      e_bs c_any  => extend_string_escaped;
      c_space+    => extend_string_space;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  plain_string := |*
      '\\' c_nl   => extend_string_eol;
      e_bs c_any  => extend_string_escaped;
      c_eol       => extend_string_eol;
      c_any       => extend_string;
  *|;

  regexp_modifiers := |*
      [A-Za-z]+
      => {
        unknown_options = tok.scan(/[^imxouesn]/)
        if unknown_options.any?
          diagnostic :error, :regexp_options,
                     { :options => unknown_options.join }
        end

        emit(:tREGEXP_OPT)
        fnext expr_end; fbreak;
      };

      any
      => {
        emit(:tREGEXP_OPT, tok(@ts, @te - 1), @ts, @te - 1)
        fhold; fgoto expr_end;
      };
  *|;

  #
  # === WHITESPACE HANDLING ===
  #

  # Various contexts in Ruby allow various kinds of whitespace
  # to be used. They are grouped to clarify the lexing machines
  # and ease collection of comments.

  # A line of code with inline #comment at end is always equivalent
  # to a line of code ending with just a newline, so an inline
  # comment is deemed equivalent to non-newline whitespace
  # (c_space character class).

  w_space =
      c_space+
    | '\\' e_heredoc_nl
    ;

  w_comment =
      '#'     %{ @sharp_s = p - 1 }
      # The (p == pe) condition compensates for added "\0" and
      # the way Ragel handles EOF.
      c_line* %{ emit_comment(@sharp_s, p == pe ? p - 2 : p) }
    ;

  w_space_comment =
      w_space
    | w_comment
    ;

  # A newline in non-literal context always interoperates with
  # here document logic and can always be escaped by a backslash,
  # still interoperating with here document logic in the same way,
  # yet being invisible to anything else.
  #
  # To demonstrate:
  #
  #     foo = <<FOO \
  #     bar
  #     FOO
  #      + 2
  #
  # is equivalent to `foo = "bar\n" + 2`.

  w_newline =
      e_heredoc_nl;

  w_any =
      w_space
    | w_comment
    | w_newline
    ;


  #
  # === EXPRESSION PARSING ===
  #

  # These rules implement a form of manually defined lookahead.
  # The default longest-match scanning does not work here due
  # to sheer ambiguity.

  ambiguous_fid_suffix =       # actual    parsed
      [?!]  %{ tm = p }      | # a?        a?
      '!='  %{ tm = p - 2 }    # a!=b      a != b
  ;

  ambiguous_ident_suffix =     # actual    parsed
      ambiguous_fid_suffix   |
      '='   %{ tm = p }      | # a=        a=
      '=='  %{ tm = p - 2 }  | # a==b      a == b
      '=~'  %{ tm = p - 2 }  | # a=~b      a =~ b
      '=>'  %{ tm = p - 2 }  | # a=>b      a => b
      '===' %{ tm = p - 3 }    # a===b     a === b
  ;

  ambiguous_symbol_suffix =    # actual    parsed
      ambiguous_ident_suffix |
      '==>' %{ tm = p - 2 }    # :a==>b    :a= => b
  ;

  # Ambiguous with 1.9 hash labels.
  ambiguous_const_suffix =     # actual    parsed
      '::'  %{ tm = p - 2 }    # A::B      A :: B
  ;

  # Resolving kDO/kDO_COND/kDO_BLOCK ambiguity requires embedding
  # @cond/@cmdarg-related code to e_lbrack, e_lparen and e_lbrace.

  e_lbrack = '[' % {
    @cond.push(false); @cmdarg.push(false)
  };

  # Ruby 1.9 lambdas require parentheses counting in order to
  # emit correct opening kDO/tLBRACE.

  e_lparen = '(' % {
    @cond.push(false); @cmdarg.push(false)

    @paren_nest += 1
  };

  e_rparen = ')' % {
    @paren_nest -= 1
  };

  # Ruby is context-sensitive wrt/ local identifiers.
  action local_ident {
    emit(:tIDENTIFIER)

    if !@static_env.nil? && @static_env.declared?(tok)
      fnext expr_end; fbreak;
    else
      fnext *arg_or_cmdarg; fbreak;
    end
  }

  # Variable lexing code is accessed from both expressions and
  # string interpolation related code.
  #
  expr_variable := |*
      global_var
      => {
        if    tok =~ /^\$([1-9][0-9]*)$/
          emit(:tNTH_REF, tok(@ts + 1).to_i)
        elsif tok =~ /^\$([&`'+])$/
          emit(:tBACK_REF)
        else
          emit(:tGVAR)
        end

        fnext *stack_pop; fbreak;
      };

      class_var_v
      => {
        if tok =~ /^@@[0-9]/
          diagnostic :error, :cvar_name, { :name => tok }
        end

        emit(:tCVAR)
        fnext *stack_pop; fbreak;
      };

      instance_var_v
      => {
        if tok =~ /^@[0-9]/
          diagnostic :error, :ivar_name, { :name => tok }
        end

        emit(:tIVAR)
        fnext *stack_pop; fbreak;
      };
  *|;

  # Literal function name in definition (e.g. `def class`).
  # Keywords are returned as their respective tokens; this is used
  # to support singleton def `def self.foo`. Global variables are
  # returned as `tGVAR`; this is used in global variable alias
  # statements `alias $a $b`. Symbols are returned verbatim; this
  # is used in `alias :a :"b#{foo}"` and `undef :a`.
  #
  # Transitions to `expr_endfn` afterwards.
  #
  expr_fname := |*
      keyword
      => { emit(KEYWORDS_BEGIN[tok]);
           fnext expr_endfn; fbreak; };

      constant
      => { emit(:tCONSTANT)
           fnext expr_endfn; fbreak; };

      bareword [?=!]?
      => { emit(:tIDENTIFIER)
           fnext expr_endfn; fbreak; };

      global_var
      => { p = @ts - 1
           fnext expr_end; fcall expr_variable; };

      # If the handling was to be delegated to expr_end,
      # these cases would transition to something else than
      # expr_endfn, which is incorrect.
      operator_fname      |
      operator_arithmetic |
      operator_rest
      => { emit_table(PUNCTUATION)
           fnext expr_endfn; fbreak; };

      '::'
      => { fhold; fhold; fgoto expr_end; };

      ':'
      => { fhold; fgoto expr_beg; };

      w_any;

      c_any
      => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  # After literal function name in definition. Behaves like `expr_end`,
  # but allows a tLABEL.
  #
  # Transitions to `expr_end` afterwards.
  #
  expr_endfn := |*
      label
      => { emit(:tLABEL, tok(@ts, @te - 1))
           fnext expr_labelarg; fbreak; };

      w_space_comment;

      c_any
      => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  # Literal function name in method call (e.g. `a.class`).
  #
  # Transitions to `expr_arg` afterwards.
  #
  expr_dot := |*
      constant
      => { emit(:tCONSTANT)
           fnext *arg_or_cmdarg; fbreak; };

      call_or_var
      => { emit(:tIDENTIFIER)
           fnext *arg_or_cmdarg; fbreak; };

      bareword ambiguous_fid_suffix
      => { emit(:tFID, tok(@ts, tm), @ts, tm)
           fnext *arg_or_cmdarg; p = tm - 1; fbreak; };

      # See the comment in `expr_fname`.
      operator_fname      |
      operator_arithmetic |
      operator_rest
      => { emit_table(PUNCTUATION)
           fnext expr_arg; fbreak; };

      w_any;

      c_any
      => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  # The previous token emitted was a `tIDENTIFIER` or `tFID`; no space
  # is consumed; the current expression is a command or method call.
  #
  expr_arg := |*
      #
      # COMMAND MODE SPECIFIC TOKENS
      #

      # cmd (1 + 2)
      # See below the rationale about expr_endarg.
      w_space+ e_lparen
      => {
        if version?(18)
          emit(:tLPAREN2, '(', @te - 1, @te)
          fnext expr_value; fbreak;
        else
          emit(:tLPAREN_ARG, '(', @te - 1, @te)
          fnext expr_beg; fbreak;
        end
      };

      # meth(1 + 2)
      # Regular method call.
      e_lparen
      => { emit(:tLPAREN2)
           fnext expr_beg; fbreak; };

      # meth [...]
      # Array argument. Compare with indexing `meth[...]`.
      w_space+ e_lbrack
      => { emit(:tLBRACK, '[', @te - 1, @te)
           fnext expr_beg; fbreak; };

      # cmd {}
      # Command: method call without parentheses.
      w_space* e_lbrace
      => {
        if @lambda_stack.last == @paren_nest
          p = @ts - 1
          fgoto expr_end;
        else
          emit(:tLCURLY, '{', @te - 1, @te)
          fnext expr_value; fbreak;
        end
      };

      #
      # AMBIGUOUS TOKENS RESOLVED VIA EXPR_BEG
      #

      # a ?b
      # Character literal.
      w_space* '?'
      => { fhold; fgoto expr_beg; };

      # a %{1}, a %[1] (but not "a %=1=" or "a % foo")
      # a /foo/ (but not "a / foo" or "a /=foo")
      # a <<HEREDOC
      w_space+ %{ tm = p }
      ( [%/] ( c_any - c_space_nl - '=' ) # /
      | '<<'
      )
      => {
        if tok(tm, tm + 1) == '/'
          # Ambiguous regexp literal.
          diagnostic :warning, :ambiguous_literal, nil, range(tm, tm + 1)
        end

        p = tm - 1
        fgoto expr_beg;
      };

      # x *1
      # Ambiguous splat, kwsplat or block-pass.
      w_space+ %{ tm = p } ( '+' | '-' | '*' | '&' | '**' )
      => {
        diagnostic :warning, :ambiguous_prefix, { :prefix => tok(tm, @te) },
                   range(tm, @te)

        p = tm - 1
        fgoto expr_beg;
      };

      # x ::Foo
      # Ambiguous toplevel constant access.
      w_space+ '::'
      => { fhold; fhold; fgoto expr_beg; };

      # x:b
      # Symbol.
      w_space* ':'
      => { fhold; fgoto expr_beg; };

      w_space+ label
      => { p = @ts - 1; fgoto expr_beg; };

      #
      # AMBIGUOUS TOKENS RESOLVED VIA EXPR_END
      #

      # a ? b
      # Ternary operator.
      w_space+ %{ tm = p } '?' c_space_nl
      => { p = tm - 1; fgoto expr_end; };

      # x + 1: Binary operator or operator-assignment.
      w_space* operator_arithmetic
                  ( '=' | c_space_nl )?    |
      # x rescue y: Modifier keyword.
      w_space* keyword_modifier            |
      # Miscellanea.
      w_space* punctuation_end
      => {
        p = @ts - 1
        fgoto expr_end;
      };

      w_space;

      w_comment
      => { fgoto expr_end; };

      w_newline
      => { fhold; fgoto expr_end; };

      c_any
      => { fhold; fgoto expr_beg; };

      c_eof => do_eof;
  *|;

  # The previous token was an identifier which was seen while in the
  # command mode (that is, the state at the beginning of #advance was
  # expr_value). This state is very similar to expr_arg, but disambiguates
  # two very rare and specific condition:
  #   * In 1.8 mode, "foo (lambda do end)".
  #   * In 1.9+ mode, "f x: -> do foo do end end".
  expr_cmdarg := |*
      w_space+ e_lparen
      => {
        emit(:tLPAREN_ARG, '(', @te - 1, @te)
        if version?(18)
          fnext expr_value; fbreak;
        else
          fnext expr_beg; fbreak;
        end
      };

      w_space* 'do'
      => {
        if @cond.active?
          emit(:kDO_COND, 'do', @te - 2, @te)
        else
          emit(:kDO, 'do', @te - 2, @te)
        end
        fnext expr_value; fbreak;
      };

      c_any             |
      # Disambiguate with the `do' rule above.
      w_space* bareword |
      w_space* label
      => { p = @ts - 1
           fgoto expr_arg; };

      c_eof => do_eof;
  *|;

  # The rationale for this state is pretty complex. Normally, if an argument
  # is passed to a command and then there is a block (tLCURLY...tRCURLY),
  # the block is attached to the innermost argument (`f` in `m f {}`), or it
  # is a parse error (`m 1 {}`). But there is a special case for passing a single
  # primary expression grouped with parentheses: if you write `m (1) {}` or
  # (2.0 only) `m () {}`, then the block is attached to `m`.
  #
  # Thus, we recognize the opening `(` of a command (remember, a command is
  # a method call without parens) as a tLPAREN_ARG; then, in parser, we recognize
  # `tLPAREN_ARG expr rparen` as a `primary_expr` and before rparen, set the
  # lexer's state to `expr_endarg`, which makes it emit the possibly following
  # `{` as `tLBRACE_ARG`.
  #
  # The default post-`expr_endarg` state is `expr_end`, so this state also handles
  # `do` (as `kDO_BLOCK` in `expr_beg`).
  expr_endarg := |*
      e_lbrace
      => { emit(:tLBRACE_ARG)
           fnext expr_value; };

      'do'
      => { emit_do(true)
           fnext expr_value; fbreak; };

      w_space_comment;

      c_any
      => { fhold; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  # The rationale for this state is that several keywords accept value
  # (i.e. should transition to `expr_beg`), do not accept it like a command
  # (i.e. not an `expr_arg`), and must behave like a statement, that is,
  # accept a modifier if/while/etc.
  #
  expr_mid := |*
      keyword_modifier
      => { emit_table(KEYWORDS)
           fnext expr_beg; fbreak; };

      bareword
      => { p = @ts - 1; fgoto expr_beg; };

      w_space_comment;

      w_newline
      => { fhold; fgoto expr_end; };

      c_any
      => { fhold; fgoto expr_beg; };

      c_eof => do_eof;
  *|;

  # Beginning of an expression.
  #
  # Don't fallthrough to this state from `c_any`; make sure to handle
  # `c_space* c_nl` and let `expr_end` handle the newline.
  # Otherwise code like `f\ndef x` gets glued together and the parser
  # explodes.
  #
  expr_beg := |*
      # Numeric processing. Converts:
      #   +5 to [tINTEGER, 5]
      #   -5 to [tUMINUS_NUM] [tINTEGER, 5]
      [+\-][0-9]
      => {
        fhold;
        if tok.start_with? '-'
          emit(:tUMINUS_NUM, '-', @ts, @ts + 1)
          fnext expr_end; fbreak;
        end
      };

      # splat *a
      '*'
      => { emit(:tSTAR)
           fbreak; };

      #
      # STRING AND REGEXP LITERALS
      #

      # /regexp/oui
      # /=/ (disambiguation with /=)
      '/' c_any
      => {
        type = delimiter = tok[0].chr
        fhold; fgoto *push_literal(type, delimiter, @ts);
      };

      # %<string>
      '%' ( any - [A-Za-z] )
      => {
        type, delimiter = tok[0].chr, tok[-1].chr
        fgoto *push_literal(type, delimiter, @ts);
      };

      # %w(we are the people)
      '%' [A-Za-z]+ c_any
      => {
        type, delimiter = tok[0..-2], tok[-1].chr
        fgoto *push_literal(type, delimiter, @ts);
      };

      '%' c_eof
      => {
        diagnostic :fatal, :string_eof, nil, range(@ts, @ts + 1)
      };

      # Heredoc start.
      # <<EOF | <<-END | <<"FOOBAR" | <<-`SMTH`
      '<<' '-'?
        ( '"' ( c_line - '"' )* '"'
        | "'" ( c_line - "'" )* "'"
        | "`" ( c_line - "`" )* "`"
        | bareword ) % { @heredoc_e     = p }
        c_line* c_nl % { new_herebody_s = p }
      => {
        tok(@ts, @heredoc_e) =~ /^<<(-?)(["'`]?)(.*)\2$/

        indent    = !$1.empty?
        type      =  '<<' + ($2.empty? ? '"' : $2)
        delimiter =  $3

        fnext *push_literal(type, delimiter, @ts, @heredoc_e, indent);

        if @herebody_s.nil?
          @herebody_s = new_herebody_s
        end

        p = @herebody_s - 1
      };

      #
      # SYMBOL LITERALS
      #

      # :"bar", :'baz'
      ':' ['"] # '
      => {
        type, delimiter = tok, tok[-1].chr
        fgoto *push_literal(type, delimiter, @ts);
      };

      ':' bareword ambiguous_symbol_suffix
      => {
        emit(:tSYMBOL, tok(@ts + 1, tm), @ts, tm)
        p = tm - 1
        fnext expr_end; fbreak;
      };

      ':' ( bareword | global_var | class_var | instance_var |
            operator_fname | operator_arithmetic | operator_rest )
      => {
        emit(:tSYMBOL, tok(@ts + 1), @ts)
        fnext expr_end; fbreak;
      };

      #
      # AMBIGUOUS TERNARY OPERATOR
      #

      '?' ( e_bs escape
          | c_any - c_space_nl - e_bs % { @escape = nil }
          )
      => {
        # Show an error if memorized.
        @escape.call if @escape.respond_to? :call

        value = @escape || tok(@ts + 1)

        if version?(18)
          emit(:tINTEGER, value[0].ord)
        else
          emit(:tCHARACTER, value)
        end

        fnext expr_end; fbreak;
      };

      '?' c_space_nl
      => {
        escape = { " "  => '\s', "\r" => '\r', "\n" => '\n', "\t" => '\t',
                   "\v" => '\v', "\f" => '\f' }[tok[1]]
        diagnostic :warning, :invalid_escape_use, { :escape => escape }, range

        p = @ts - 1
        fgoto expr_end;
      };

      '?' c_eof
      => {
        diagnostic :fatal, :incomplete_escape, nil, range(@ts, @ts + 1)
      };

      # f ?aa : b: Disambiguate with a character literal.
      '?' [A-Za-z_] bareword
      => {
        p = @ts - 1
        fgoto expr_end;
      };

      #
      # KEYWORDS AND PUNCTUATION
      #

      # a({b=>c})
      e_lbrace
      => {
        if @lambda_stack.last == @paren_nest
          @lambda_stack.pop
          emit(:tLAMBEG)
        else
          emit_table(PUNCTUATION_BEGIN)
        end
        fbreak;
      };

      # a([1, 2])
      e_lbrack    |
      # a()
      e_lparen
      => { emit_table(PUNCTUATION_BEGIN)
           fbreak; };

      # a(+b)
      punctuation_begin
      => { emit_table(PUNCTUATION_BEGIN)
           fbreak; };

      # rescue Exception => e: Block rescue.
      # Special because it should transition to expr_mid.
      'rescue' %{ tm = p } '=>'?
      => { emit_table(KEYWORDS_BEGIN, @ts, tm)
           p = tm - 1
           fnext expr_mid; fbreak; };

      # if a: Statement if.
      keyword_modifier
      => { emit_table(KEYWORDS_BEGIN)
           fnext expr_value; fbreak; };

      #
      # RUBY 1.9 HASH LABELS
      #

      label ( any - ':' )
      => {
        fhold;

        if version?(18)
          ident = tok(@ts, @te - 2)

          emit((tok[0] =~ /[A-Z]/) ? :tCONSTANT : :tIDENTIFIER,
               ident, @ts, @te - 2)
          fhold; # continue as a symbol

          if !@static_env.nil? && @static_env.declared?(ident)
            fnext expr_end;
          else
            fnext *arg_or_cmdarg;
          end
        else
          emit(:tLABEL, tok(@ts, @te - 2), @ts, @te - 1)
          fnext expr_labelarg;
        end

        fbreak;
      };

      #
      # CONTEXT-DEPENDENT VARIABLE LOOKUP OR COMMAND INVOCATION
      #

      # foo= bar:  Disambiguate with bareword rule below.
      bareword ambiguous_ident_suffix |
      # def foo:   Disambiguate with bareword rule below.
      keyword
      => { p = @ts - 1
           fgoto expr_end; };

      # a = 42;     a [42]: Indexing.
      # def a; end; a [42]: Array argument.
      call_or_var
      => local_ident;

      #
      # WHITESPACE
      #

      w_any;

      e_heredoc_nl '=begin' ( c_space | c_nl_zlen )
      => { p = @ts - 1
           fgoto line_begin; };

      #
      # DEFAULT TRANSITION
      #

      # The following rules match most binary and all unary operators.
      # Rules for binary operators provide better error reporting.
      operator_arithmetic '='    |
      operator_rest              |
      punctuation_end            |
      c_any
      => { p = @ts - 1; fgoto expr_end; };

      c_eof => do_eof;
  *|;

  # Special newline handling for "def a b:"
  #
  expr_labelarg := |*
    w_space_comment;

    w_newline
    => {
      if @in_kwarg
        fhold; fgoto expr_end;
      else
        fgoto line_begin;
      end
    };

    c_any
    => { fhold; fgoto expr_beg; };

    c_eof => do_eof;
  *|;

  # Like expr_beg, but no 1.9 label possible.
  #
  expr_value := |*
      # a:b: a(:b), a::B, A::B
      label (any - ':')
      => { p = @ts - 1
           fgoto expr_end; };

      w_space_comment;

      w_newline
      => { fgoto line_begin; };

      c_any
      => { fhold; fgoto expr_beg; };

      c_eof => do_eof;
  *|;

  expr_end := |*
      #
      # STABBY LAMBDA
      #

      '->'
      => {
        emit_table(PUNCTUATION, @ts, @ts + 2)

        @lambda_stack.push @paren_nest
        fbreak;
      };

      e_lbrace | 'do'
      => {
        if @lambda_stack.last == @paren_nest
          @lambda_stack.pop

          if tok == '{'
            emit(:tLAMBEG)
          else # 'do'
            emit(:kDO_LAMBDA)
          end
        else
          if tok == '{'
            emit_table(PUNCTUATION)
          else # 'do'
            emit_do
          end
        end

        fnext expr_value; fbreak;
      };

      #
      # KEYWORDS
      #

      keyword_with_fname
      => { emit_table(KEYWORDS)
           fnext expr_fname; fbreak; };

      'class' w_any* '<<'
      => { emit(:kCLASS, 'class', @ts, @ts + 5)
           emit(:tLSHFT, '<<',    @te - 2, @te)
           fnext expr_value; fbreak; };

      # a if b:c: Syntax error.
      keyword_modifier
      => { emit_table(KEYWORDS)
           fnext expr_beg; fbreak; };

      # elsif b:c: elsif b(:c)
      keyword_with_value
      => { emit_table(KEYWORDS)
           fnext expr_value; fbreak; };

      keyword_with_mid
      => { emit_table(KEYWORDS)
           fnext expr_mid; fbreak; };

      keyword_with_arg
      => {
        emit_table(KEYWORDS)

        if version?(18) && tok == 'not'
          fnext expr_beg; fbreak;
        else
          fnext expr_arg; fbreak;
        end
      };

      '__ENCODING__'
      => {
        if version?(18)
          emit(:tIDENTIFIER)

          if !@static_env.nil? && @static_env.declared?(tok)
            fnext expr_end;
          else
            fnext *arg_or_cmdarg;
          end
        else
          emit_table(KEYWORDS)
        end
        fbreak;
      };

      keyword_with_end
      => { emit_table(KEYWORDS)
           fbreak; };

      #
      # NUMERIC LITERALS
      #

      ( '0' [Xx] %{ @num_base = 16; @num_digits_s = p } int_hex
      | '0' [Dd] %{ @num_base = 10; @num_digits_s = p } int_dec
      | '0' [Oo] %{ @num_base = 8;  @num_digits_s = p } int_dec
      | '0' [Bb] %{ @num_base = 2;  @num_digits_s = p } int_bin
      | [1-9] digit* '_'? %{ @num_base = 10; @num_digits_s = @ts } int_dec
      | '0'   digit* '_'? %{ @num_base = 8;  @num_digits_s = @ts } int_dec
      ) %{ @num_suffix_s = p } int_suffix
      => {
        digits = tok(@num_digits_s, @num_suffix_s)

        if digits.end_with? '_'
          diagnostic :error, :trailing_in_number, { :character => '_' },
                     range(@te - 1, @te)
        elsif digits.empty? && @num_base == 8 && version?(18)
          # 1.8 did not raise an error on 0o.
          digits = "0"
        elsif digits.empty?
          diagnostic :error, :empty_numeric
        elsif @num_base == 8 && (invalid_idx = digits.index(/[89]/))
          invalid_s = @num_digits_s + invalid_idx
          diagnostic :error, :invalid_octal, nil,
                     range(invalid_s, invalid_s + 1)
        end

        if version?(18, 19, 20)
          emit(:tINTEGER, digits.to_i(@num_base))
          p = @num_suffix_s - 1
        else
          @num_xfrm.call(digits.to_i(@num_base))
        end
        fbreak;
      };

      flo_frac flo_pow?
      => {
        diagnostic :error, :no_dot_digit_literal
      };

      flo_int [eE]
      => {
        if version?(18, 19, 20)
          diagnostic :error,
                     :trailing_in_number, { :character => tok(@te - 1, @te) },
                     range(@te - 1, @te)
        else
          emit(:tINTEGER, tok(@ts, @te - 1).to_i)
          fhold; fbreak;
        end
      };

      flo_int flo_frac [eE]
      => {
        if version?(18, 19, 20)
          diagnostic :error,
                     :trailing_in_number, { :character => tok(@te - 1, @te) },
                     range(@te - 1, @te)
        else
          emit(:tFLOAT, tok(@ts, @te - 1).to_f)
          fhold; fbreak;
        end
      };

      flo_int
      ( flo_frac? flo_pow %{ @num_suffix_s = p } flo_pow_suffix
      | flo_frac          %{ @num_suffix_s = p } flo_suffix
      )
      => {
        digits = tok(@ts, @num_suffix_s)

        if version?(18, 19, 20)
          emit(:tFLOAT, Float(digits))
          p = @num_suffix_s - 1
        else
          @num_xfrm.call(digits)
        end
        fbreak;
      };

      #
      # STRING AND XSTRING LITERALS
      #

      # `echo foo`, "bar", 'baz'
      '`' | ['"] # '
      => {
        type, delimiter = tok, tok[-1].chr
        fgoto *push_literal(type, delimiter, @ts);
      };

      #
      # CONSTANTS AND VARIABLES
      #

      constant
      => { emit(:tCONSTANT)
           fnext *arg_or_cmdarg; fbreak; };

      constant ambiguous_const_suffix
      => { emit(:tCONSTANT, tok(@ts, tm), @ts, tm)
           p = tm - 1; fbreak; };

      global_var | class_var_v | instance_var_v
      => { p = @ts - 1; fcall expr_variable; };

      #
      # METHOD CALLS
      #

      '.' | '::'
      => { emit_table(PUNCTUATION)
           fnext expr_dot; fbreak; };

      call_or_var
      => local_ident;

      bareword ambiguous_fid_suffix
      => {
        if tm == @te
          # Suffix was consumed, e.g. foo!
          emit(:tFID)
        else
          # Suffix was not consumed, e.g. foo!=
          emit(:tIDENTIFIER, tok(@ts, tm), @ts, tm)
          p = tm - 1
        end
        fnext expr_arg; fbreak;
      };

      #
      # OPERATORS
      #

      ( e_lparen
      | operator_arithmetic
      | operator_rest
      )
      => { emit_table(PUNCTUATION)
           fnext expr_beg; fbreak; };

      e_rbrace | e_rparen | ']'
      => {
        emit_table(PUNCTUATION)
        @cond.lexpop; @cmdarg.lexpop

        if %w"} ]".include?(tok)
          fnext expr_endarg;
        else # )
          # fnext expr_endfn; ?
        end

        fbreak;
      };

      operator_arithmetic '='
      => { emit(:tOP_ASGN, tok(@ts, @te - 1))
           fnext expr_beg; fbreak; };

      '?'
      => { emit_table(PUNCTUATION)
           fnext expr_value; fbreak; };

      e_lbrack
      => { emit_table(PUNCTUATION)
           fnext expr_beg; fbreak; };

      punctuation_end
      => { emit_table(PUNCTUATION)
           fnext expr_beg; fbreak; };

      #
      # WHITESPACE
      #

      w_space_comment;

      w_newline
      => { fgoto leading_dot; };

      ';'
      => { emit_table(PUNCTUATION)
           fnext expr_value; fbreak; };

      '\\' c_line {
        diagnostic :error, :bare_backslash, nil, range(@ts, @ts + 1)
        fhold;
      };

      c_any
      => {
        diagnostic :fatal, :unexpected, { :character => tok.inspect[1..-2] }
      };

      c_eof => do_eof;
  *|;

  leading_dot := |*
      # Insane leading dots:
      # a #comment
      #  .b: a.b
      c_space* '.' ( c_any - '.' )
      => { fhold; fhold;
           fgoto expr_end; };

      any
      => { emit(:tNL, nil, @newline_s, @newline_s + 1)
           fhold; fnext line_begin; fbreak; };
  *|;

  #
  # === EMBEDDED DOCUMENT (aka BLOCK COMMENT) PARSING ===
  #

  line_comment := |*
      '=end' c_line* c_nl_zlen
      => {
        emit_comment(@eq_begin_s, @te)
        fgoto line_begin;
      };

      c_line* c_nl;

      c_line* zlen
      => {
        diagnostic :fatal, :embedded_document, nil,
                   range(@eq_begin_s, @eq_begin_s + '=begin'.length)
      };
  *|;

  line_begin := |*
      w_any;

      '=begin' ( c_space | c_nl_zlen )
      => { @eq_begin_s = @ts
           fgoto line_comment; };

      '__END__' ( c_eol - zlen )
      => { p = pe - 3 };

      c_any
      => { fhold; fgoto expr_value; };

      c_eof => do_eof;
  *|;

  }%%
  # %
end
