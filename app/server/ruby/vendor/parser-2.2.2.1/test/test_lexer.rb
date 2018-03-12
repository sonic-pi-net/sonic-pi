# encoding: ascii-8bit

require 'helper'
require 'complex'

class TestLexer < Minitest::Test
  def setup_lexer(version)
    @lex = Parser::Lexer.new(version)

    @lex.comments = []
    @lex.diagnostics = Parser::Diagnostic::Engine.new
    @lex.diagnostics.all_errors_are_fatal = true
    # @lex.diagnostics.consumer = lambda { |diag| $stderr.puts "", diag.render }
  end

  def setup
    setup_lexer 18
  end

  #
  # Tools
  #

  def utf(str)
    if str.respond_to?(:force_encoding)
      str.force_encoding(Encoding::UTF_8)
    else
      str
    end
  end

  #
  # Additional matchers
  #

  def refute_scanned(s, *args)
    assert_raises Parser::SyntaxError do
      assert_scanned(s, *args)
    end
  end

  def assert_escape(expected, input)
    source_buffer = Parser::Source::Buffer.new('(assert_escape)')

    if defined?(Encoding)
      source_buffer.source = "\"\\#{input}\"".encode(input.encoding)
    else
      source_buffer.source = "\"\\#{input}\""
    end

    @lex.reset
    @lex.source_buffer = source_buffer

    lex_token, (lex_value, *) = @lex.advance

    if lex_value.respond_to?(:force_encoding)
      lex_value.force_encoding(Encoding::BINARY)
    end

    assert_equal [:tSTRING, expected],
                 [lex_token, lex_value],
                 source_buffer.source
  end

  def refute_escape(input)
    err = assert_raises Parser::SyntaxError do
      @lex.state = :expr_beg
      assert_scanned "%Q[\\#{input}]"
    end
    assert_equal :fatal, err.diagnostic.level
  end

  def assert_lex_fname(name, type)
    assert_scanned("def #{name} ", :kDEF, 'def', type, name)

    assert_equal :expr_endfn, @lex.state
  end

  def assert_scanned(input, *args)
    source_buffer = Parser::Source::Buffer.new('(assert_scanned)')
    source_buffer.source = input

    @lex.reset(false)
    @lex.source_buffer = source_buffer

    until args.empty? do
      token, value = args.shift(2)

      lex_token, (lex_value, *) = @lex.advance
      assert lex_token, 'no more tokens'
      assert_operator [lex_token, lex_value], :eql?, [token, value], input
    end

    lex_token, (lex_value, *) = @lex.advance
    refute lex_token, "must be empty, but had #{[lex_token, lex_value].inspect}"
  end

  #
  # Tests
  #

  def test_read_escape
    assert_escape "\\",   "\\"
    assert_escape "\n",   "n"
    assert_escape "\t",   "t"
    assert_escape "\r",   "r"
    assert_escape "\f",   "f"
    assert_escape "\13",  "v"
    assert_escape "\0",   "0"
    assert_escape "\07",  "a"
    assert_escape "\007", "a"
    assert_escape "\033", "e"
    assert_escape "\377", "377"
    assert_escape "\377", "xff"
    assert_escape "\010", "b"
    assert_escape " ",    "s"
    assert_escape "q",    "q" # plain vanilla escape
  end

  def test_read_escape_c
    assert_escape "\030", "C-x"
    assert_escape "\030", "cx"
    assert_escape "\230", 'C-\M-x'
    assert_escape "\230", 'c\M-x'

    assert_escape "\177", "C-?"
    assert_escape "\177", "c?"
    assert_escape "\r",   "cM"
  end

  def test_read_escape_m
    assert_escape "\370", "M-x"
    assert_escape "\230", 'M-\C-x'
    assert_escape "\230", 'M-\cx'
  end

  def test_read_escape_errors
    refute_escape ""

    refute_escape "M"
    refute_escape "M-"
    refute_escape "Mx"

    refute_escape "Cx"
    refute_escape "C"
    refute_escape "C-"

    refute_escape "c"

    refute_escape "x"
  end

  def test_read_escape_unicode__19
    if RUBY_VERSION >= '1.9'
      assert_escape "\x09", 'u{9}'
      assert_escape "\x31", 'u{31}'
      assert_escape "\x09\x01", 'u{9 1}'

      assert_escape "\xc4\xa3", utf('u0123')
      assert_escape "\xc4\xa3\xc3\xb0\xeb\x84\xa3", utf('u{123 f0 B123}')
    end
  end

  def test_read_escape_unicode_bad__19
    if RUBY_VERSION >= '1.9'
      refute_escape 'u123'
      refute_escape 'u{}'
      refute_escape 'u{123 f0h}'
      refute_escape 'u{123 f0'
    end
  end

  def test_ambiguous_uminus
    assert_scanned("m -3",
                   :tIDENTIFIER, "m",
                   :tUMINUS_NUM, "-",
                   :tINTEGER, 3)
  end

  def test_ambiguous_uplus
    assert_scanned("m +3",
                   :tIDENTIFIER, "m",
                   :tINTEGER, 3)
  end

  def test_and
    assert_scanned "&", :tAMPER, "&"
  end

  def test_and2
    @lex.state = :expr_end

    assert_scanned "&&", :tANDOP, "&&"
  end

  def test_and2_equals
    @lex.state = :expr_end

    assert_scanned "&&=", :tOP_ASGN, "&&"
  end

  def test_and_arg
    @lex.state = :expr_arg

    assert_scanned(" &y",
                   :tAMPER, "&",
                   :tIDENTIFIER, "y")
  end

  def test_and_equals
    @lex.state = :expr_end

    assert_scanned "&=", :tOP_ASGN, "&"
  end

  def test_and_expr
    @lex.state = :expr_arg

    assert_scanned("x & y",
                   :tIDENTIFIER, "x",
                   :tAMPER2, "&",
                   :tIDENTIFIER, "y")
  end

  def test_and_meth
    assert_lex_fname "&", :tAMPER2
  end

  def test_assoc
    assert_scanned "=>", :tASSOC, "=>"
  end

  def test_label__18
    assert_scanned("{a:b",
                   :tLBRACE,     "{",
                   :tIDENTIFIER, "a",
                   :tSYMBOL,     "b")
  end

  def test_label_in_params__18
    assert_scanned("foo(a:b",
                   :tIDENTIFIER, "foo",
                   :tLPAREN2,    "(",
                   :tIDENTIFIER, "a",
                   :tSYMBOL,     "b")
  end

  def test_label__19
    setup_lexer 19

    assert_scanned("{a:b",
                   :tLBRACE,     "{",
                   :tLABEL,      "a",
                   :tIDENTIFIER, "b")
  end

  def test_label_in_params__19
    setup_lexer 19

    assert_scanned("foo(a:b",
                   :tIDENTIFIER, "foo",
                   :tLPAREN2,    "(",
                   :tLABEL,      "a",
                   :tIDENTIFIER, "b")
  end

  def test_label_fid__19
    setup_lexer 19

    assert_scanned("{a?:true",
                   :tLBRACE,     '{',
                   :tLABEL,      'a?',
                   :kTRUE,       'true')
  end

  def test_label__22
    setup_lexer 22

    assert_scanned("{'a':",
                   :tLBRACE,          '{',
                   :tSTRING_BEG,      "'",
                   :tSTRING_CONTENT,  'a',
                   :tLABEL_END,       "'")
  end

  def test_label_nested__22
    setup_lexer 22

    assert_scanned("{'a\":':",
                   :tLBRACE,          '{',
                   :tSTRING_BEG,      "'",
                   :tSTRING_CONTENT,  'a":',
                   :tLABEL_END,       "'")
  end

  def test_label_colon2__22
    setup_lexer 22

    assert_scanned("{'a'::",
                   :tLBRACE, '{',
                   :tSTRING, "a",
                   :tCOLON2, '::')
  end

  def test_command_start__19
    setup_lexer 19

    %w[case elsif for in until when while
      if unless and or].each do |keyword|
      token = "k#{keyword.upcase}".to_sym

      @lex.reset
      assert_scanned("#{keyword} a:b",
                     token,         keyword,
                     :tIDENTIFIER,  "a",
                     :tSYMBOL,      "b")
    end
  end

  def test_mod_not_command_start__19
    setup_lexer 19

    %w[if unless while until rescue].each do |keyword|
      token = "k#{keyword.upcase}_MOD".to_sym

      @lex.state = :expr_end
      assert_scanned("#{keyword} a:b",
                     token,         keyword,
                     :tLABEL,       "a",
                     :tIDENTIFIER,  "b")
    end
  end

  def test_back_ref
    assert_scanned("[$&, $`, $', $+]",
                   :tLBRACK,   "[",
                   :tBACK_REF, "$&", :tCOMMA, ",",
                   :tBACK_REF, "$`", :tCOMMA, ",",
                   :tBACK_REF, "$'", :tCOMMA, ",",
                   :tBACK_REF, "$+",
                   :tRBRACK,   "]")
  end

  def test_backslash
    assert_scanned("1 \\\n+ 2",
                   :tINTEGER, 1,
                   :tPLUS, "+",
                   :tINTEGER, 2)
  end

  def test_backslash_bad
    refute_scanned("1 \\ + 2",
                   :tINTEGER, 1)
  end

  def test_backtick
    assert_scanned("`ls`",
                   :tXSTRING_BEG, "`",
                   :tSTRING_CONTENT, "ls",
                   :tSTRING_END, "`")
  end

  def test_backtick_cmdarg
    @lex.state = :expr_dot
    assert_scanned("\n`", :tBACK_REF2, "`") # \n ensures expr_cmd

    assert_equal :expr_arg, @lex.state
  end

  def test_backtick_dot
    @lex.state = :expr_dot
    assert_scanned("a.`(3)",
                   :tIDENTIFIER, "a",
                   :tDOT, ".",
                   :tBACK_REF2, "`",
                   :tLPAREN2, "(",
                   :tINTEGER, 3,
                   :tRPAREN, ")")
  end

  def test_backtick_method
    @lex.state = :expr_fname
    assert_scanned("`", :tBACK_REF2, "`")
    assert_equal :expr_endfn, @lex.state
  end

  def test_bad_char
    refute_scanned(" \010 ")
  end

  def test_bang
    assert_scanned "!", :tBANG, "!"
  end

  def test_bang_equals
    assert_scanned "!=", :tNEQ, "!="
  end

  def test_bang_tilde
    assert_scanned "!~", :tNMATCH, "!~"
  end

  def test_def_ubang
    setup_lexer(20)

    @lex.state = :expr_fname
    assert_scanned '!@', :tBANG, '!@'
  end

  def test_carat
    assert_scanned "^", :tCARET, "^"
  end

  def test_carat_equals
    assert_scanned "^=", :tOP_ASGN, "^"
  end

  def test_colon2
    assert_scanned("A::B",
                   :tCONSTANT, "A",
                   :tCOLON2,   "::",
                   :tCONSTANT, "B")

    @lex.state = :expr_arg
    assert_scanned("::Array",
                   :tCOLON2, "::",
                   :tCONSTANT, "Array")
  end

  def test_colon3
    assert_scanned("::Array",
                   :tCOLON3, "::",
                   :tCONSTANT, "Array")

    @lex.state = :expr_arg
    assert_scanned(" ::Array",
                   :tCOLON3, "::",
                   :tCONSTANT, "Array")
  end

  def test_comma
    assert_scanned ",", :tCOMMA, ","
  end

  def test_comment
    assert_scanned("1 # one\n# two\n2",
                   :tINTEGER, 1,
                   :tNL, nil,
                   :tINTEGER, 2)

    assert_equal 2, @lex.comments.length
    assert_equal '# one', @lex.comments[0].text
    assert_equal '# two', @lex.comments[1].text
  end

  def test_comment_expr_beg
    assert_scanned("{#1\n}",
                   :tLBRACE, "{",
                   :tRCURLY, "}")
  end

  def test_comment_begin
    assert_scanned("=begin\nblah\nblah\n=end\n42",
                   :tINTEGER, 42)
    assert_equal 1, @lex.comments.length
    assert_equal "=begin\nblah\nblah\n=end\n", @lex.comments[0].text
  end

  def test_comment_begin_bad
    refute_scanned("=begin\nblah\nblah\n")
  end

  def test_comment_begin_not_comment
    assert_scanned("beginfoo = 5\np x \\\n=beginfoo",
                   :tIDENTIFIER, "beginfoo",
                   :tEQL,          "=",
                   :tINTEGER,    5,
                   :tNL,         nil,
                   :tIDENTIFIER, "p",
                   :tIDENTIFIER, "x",
                   :tEQL,          "=",
                   :tIDENTIFIER, "beginfoo")
  end

  def test_comment_begin_space
    assert_scanned("=begin blah\nblah\n=end\n")

    assert_equal 1, @lex.comments.length
    assert_equal "=begin blah\nblah\n=end\n", @lex.comments[0].text
  end

  def test_comment_end_space_and_text
    assert_scanned("=begin blah\nblah\n=end blab\n")

    assert_equal 1, @lex.comments.length
    assert_equal "=begin blah\nblah\n=end blab\n", @lex.comments[0].text
  end

  def test_comment_eos
    assert_scanned("# comment")
  end

  def test_constant
    assert_scanned("ArgumentError",
                   :tCONSTANT, "ArgumentError")
  end

  def test_constant_semi
    assert_scanned("ArgumentError;",
                   :tCONSTANT, "ArgumentError",
                   :tSEMI, ";")
  end

  def test_cvar
    assert_scanned "@@blah", :tCVAR, "@@blah"
  end

  def test_cvar_bad
    refute_scanned "@@1"
  end

  def test_div
    assert_scanned("a / 2",
                   :tIDENTIFIER, "a",
                   :tDIVIDE, "/",
                   :tINTEGER, 2)
  end

  def test_div_equals
    assert_scanned("a /= 2",
                   :tIDENTIFIER, "a",
                   :tOP_ASGN, "/",
                   :tINTEGER, 2)
  end

  def test_do
    assert_scanned("x do 42 end",
                   :tIDENTIFIER, "x",
                   :kDO, "do",
                   :tINTEGER, 42,
                   :kEND, "end")
  end

  def test_do_cond
    @lex.cond.push(true)

    assert_scanned("x do 42 end",
                   :tIDENTIFIER, "x",
                   :kDO_COND, "do",
                   :tINTEGER, 42,
                   :kEND, "end")
  end

  def test_do_block
    @lex.state = :expr_endarg

    assert_scanned("do 42 end",
                   :kDO_BLOCK, "do",
                   :tINTEGER, 42,
                   :kEND, "end")
  end

  def test_do_cond
    @lex.cond.push true

    assert_scanned("x do 42 end",
                   :tIDENTIFIER, "x",
                   :kDO_COND, "do",
                   :tINTEGER, 42,
                   :kEND, "end")
  end

  def test_dot
    assert_scanned ".", :tDOT, "."
  end

  def test_dot2
    assert_scanned "..", :tDOT2, ".."
  end

  def test_dot3
    assert_scanned "...", :tDOT3, "..."
  end

  def test_equals
    assert_scanned "=", :tEQL, "="
  end

  def test_equals2
    assert_scanned "==", :tEQ, "=="
  end

  def test_equals3
    assert_scanned "===", :tEQQ, "==="
  end

  def test_equals_tilde
    assert_scanned "=~", :tMATCH, "=~"
  end

  def test_float
    assert_scanned "1.0", :tFLOAT, 1.0
  end

  def test_float_bad_no_underscores
    refute_scanned "1__0.0"
  end

  def test_float_bad_no_zero_leading
    refute_scanned ".0"
  end

  def test_float_bad_trailing_underscore
    refute_scanned "123_.0"
  end

  def test_float_call
    assert_scanned("1.0.to_s",
                   :tFLOAT, 1.0,
                   :tDOT, ".",
                   :tIDENTIFIER, "to_s")
  end

  def test_float_dot_E
    assert_scanned "1.0E10", :tFLOAT, 1.0e10
  end

  def test_float_dot_E_neg
    assert_scanned("-1.0E10",
                   :tUMINUS_NUM, "-",
                   :tFLOAT, 1.0e10)
  end

  def test_float_dot_e
    assert_scanned "1.0e10", :tFLOAT, 1.0e10
  end

  def test_float_dot_e_neg
    assert_scanned("-1.0e10",
                   :tUMINUS_NUM, "-",
                   :tFLOAT, 1.0e10)
  end

  def test_float_e
    assert_scanned "1e10", :tFLOAT, 1e10
  end

  def test_float_e_bad_trailing_underscore
    refute_scanned "123_e10"
  end

  def test_float_e_minus
    assert_scanned "1e-10", :tFLOAT, 1e-10
  end

  def test_float_e_neg
    assert_scanned("-1e10",
                   :tUMINUS_NUM, "-",
                   :tFLOAT, 1e10)
  end

  def test_float_e_neg_minus
    assert_scanned("-1e-10",
                   :tUMINUS_NUM, "-",
                   :tFLOAT, 1e-10)
  end

  def test_float_e_neg_plus
    assert_scanned("-1e+10",
                   :tUMINUS_NUM, "-",
                   :tFLOAT, 1e10)
  end

  def test_float_e_plus
    assert_scanned "1e+10", :tFLOAT, 1e10
  end

  def test_float_e_zero
    assert_scanned "0e0", :tFLOAT, 0e0
  end

  def test_float_e_nothing
    [18, 19, 20].each do |version|
      setup_lexer version

      refute_scanned "1end"
      refute_scanned "1.1end"
    end

    setup_lexer 21

    assert_scanned("1end",
                   :tINTEGER, 1,
                   :kEND,     'end')
    assert_scanned("1.1end",
                   :tFLOAT,   1.1,
                   :kEND,     'end')
  end

  def test_float_neg
    assert_scanned("-1.0",
                   :tUMINUS_NUM, "-",
                   :tFLOAT, 1.0)
  end

  def test_ge
    assert_scanned("a >= 2",
                   :tIDENTIFIER, "a",
                   :tGEQ, ">=",
                   :tINTEGER, 2)
  end

  def test_global
    assert_scanned("$blah", :tGVAR, "$blah")
  end

  def test_global_backref
    assert_scanned("$`", :tBACK_REF, "$`")
  end

  # This was removed in 2.1.
  # def test_global_dash_nothing
  #   assert_scanned("$- ", :tGVAR, "$-")
  # end

  def test_global_dash_something
    assert_scanned("$-x", :tGVAR, "$-x")
  end

  def test_global_number
    assert_scanned("$10", :tNTH_REF, 10)
  end

  def test_global_other
    assert_scanned("[$~, $*, $$, $?, $!, $@, $/, $\\, $;, $,, $., $=, $:, $<, $>, $\"]",
                   :tLBRACK, "[",
                   :tGVAR,   "$~",  :tCOMMA, ",",
                   :tGVAR,   "$*",  :tCOMMA, ",",
                   :tGVAR,   "$$",  :tCOMMA, ",",
                   :tGVAR,   "$\?",  :tCOMMA, ",",
                   :tGVAR,   "$!",  :tCOMMA, ",",
                   :tGVAR,   "$@",  :tCOMMA, ",",
                   :tGVAR,   "$/",  :tCOMMA, ",",
                   :tGVAR,   "$\\", :tCOMMA, ",",
                   :tGVAR,   "$;",  :tCOMMA, ",",
                   :tGVAR,   "$,",  :tCOMMA, ",",
                   :tGVAR,   "$.",  :tCOMMA, ",",
                   :tGVAR,   "$=",  :tCOMMA, ",",
                   :tGVAR,   "$:",  :tCOMMA, ",",
                   :tGVAR,   "$<",  :tCOMMA, ",",
                   :tGVAR,   "$>",  :tCOMMA, ",",
                   :tGVAR,   "$\"",
                   :tRBRACK, "]")
  end

  def test_global_underscore
    assert_scanned("$_",
                   :tGVAR,     "$_")
  end

  def test_global_wierd
    assert_scanned("$__blah",
                   :tGVAR,     "$__blah")
  end

  def test_global_zero
    assert_scanned("$0", :tGVAR, "$0")
  end

  def test_gt
    assert_scanned("a > 2",
                   :tIDENTIFIER, "a",
                   :tGT, ">",
                   :tINTEGER, 2)
  end

  def test_heredoc_backtick
    assert_scanned("a = <<`EOF`\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tXSTRING_BEG,    "<<`",
                   :tSTRING_CONTENT, "  blah blah\n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_double
    assert_scanned("a = <<\"EOF\"\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "  blah blah\n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_double_dash
    assert_scanned("a = <<-\"EOF\"\n  blah blah\n  EOF\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "  blah blah\n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_double_eos
    refute_scanned("a = <<\"EOF\"\nblah",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"")
  end

  def test_heredoc_double_eos_nl
    refute_scanned("a = <<\"EOF\"\nblah\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"")
  end

  def test_heredoc_double_interp
    assert_scanned("a = <<\"EOF\"\n#x a \#@a b \#$b c \#{3} \nEOF\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "#x a ",
                   :tSTRING_DVAR,    nil,
                   :tIVAR,           "@a",
                   :tSTRING_CONTENT, " b ",
                   :tSTRING_DVAR,    nil,
                   :tGVAR,           "$b",
                   :tSTRING_CONTENT, " c ",
                   :tSTRING_DBEG,    '#{',
                   :tINTEGER,        3,
                   :tRCURLY,         "}",
                   :tSTRING_CONTENT, " \n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_empty
    assert_scanned("<<\"\"\n\#{x}\nblah2\n\n",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_DBEG,    "\#{",
                   :tIDENTIFIER,     "x",
                   :tRCURLY,         "}",
                   :tSTRING_CONTENT, "\n",
                   :tSTRING_CONTENT, "blah2\n",
                   :tSTRING_END,     "",
                   :tNL,             nil)
  end

  def test_heredoc_none
    assert_scanned("a = <<EOF\nblah\nblah\nEOF",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "blah\n",
                   :tSTRING_CONTENT, "blah\n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_none_dash
    assert_scanned("a = <<-EOF\nblah\nblah\n  EOF",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "blah\n",
                   :tSTRING_CONTENT, "blah\n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_single
    assert_scanned("a = <<'EOF'\n  blah blah\nEOF\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<'",
                   :tSTRING_CONTENT, "  blah blah\n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_single_bad_eos_body
    refute_scanned("a = <<'EOF'\nblah",
                   :tIDENTIFIER,     "a",
                   :tEQL,              "=",
                   :tSTRING_BEG,     "'")
  end

  def test_heredoc_single_dash
    assert_scanned("a = <<-'EOF'\n  blah blah\n  EOF\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<'",
                   :tSTRING_CONTENT, "  blah blah\n",
                   :tSTRING_END,     "EOF",
                   :tNL,             nil)
  end

  def test_heredoc_one_character
    assert_scanned("a = <<E\nABCDEF\nE\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "ABCDEF\n",
                   :tSTRING_END,     "E",
                   :tNL,             nil)
  end

  def test_heredoc_cr
    assert_scanned("a = <<E\r\r\nABCDEF\r\r\nE\r\r\r\n",
                   :tIDENTIFIER,     "a",
                   :tEQL,            "=",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "ABCDEF\r\n",
                   :tSTRING_END,     "E",
                   :tNL,             nil)
  end

  def test_identifier
    assert_scanned("identifier",
                   :tIDENTIFIER, "identifier")
  end

  def test_identifier_bang
    assert_scanned("identifier!",
                   :tFID,        "identifier!")

    assert_scanned("identifier!=",
                   :tIDENTIFIER, "identifier",
                   :tNEQ,        "!=")
  end

  def test_identifier_cmp
    assert_lex_fname "<=>", :tCMP
  end

  def test_identifier_def
    assert_lex_fname "identifier", :tIDENTIFIER
  end

  def test_identifier_eh
    assert_scanned("identifier?", :tFID, "identifier?")
  end

  def test_identifier_equals_arrow
    assert_scanned(":blah==>",
                   :tSYMBOL, "blah=",
                   :tASSOC, "=>")
  end

  def test_identifier_equals3
    assert_scanned(":a===b",
                   :tSYMBOL, "a",
                   :tEQQ, "===",
                   :tIDENTIFIER, "b")
  end

  def test_identifier_equals_equals_arrow
    assert_scanned(":a==>b",
                   :tSYMBOL, "a=",
                   :tASSOC, "=>",
                   :tIDENTIFIER, "b")
  end

  def test_identifier_equals_caret
    assert_lex_fname "^", :tCARET
  end

  def test_identifier_equals_def
    assert_lex_fname "identifier=", :tIDENTIFIER
  end

  def test_identifier_equals_def2
    assert_lex_fname "==", :tEQ
  end

  def test_identifier_equals_expr
    @lex.state = :expr_dot
    assert_scanned("y = arg",
                   :tIDENTIFIER, "y",
                   :tEQL, "=",
                   :tIDENTIFIER, "arg")

    assert_equal :expr_arg, @lex.state
  end

  def test_identifier_equals_or
    assert_lex_fname "|", :tPIPE
  end

  def test_identifier_equals_slash
    assert_lex_fname "/", :tDIVIDE
  end

  def test_identifier_equals_tilde
    @lex.state = :expr_fname
    assert_scanned("identifier=~",
                   :tIDENTIFIER, "identifier=",
                   :tTILDE,      "~")
  end

  def test_identifier_gt
    assert_lex_fname ">", :tGT
  end

  def test_identifier_le
    assert_lex_fname "<=", :tLEQ
  end

  def test_identifier_lt
    assert_lex_fname "<", :tLT
  end

  def test_identifier_tilde
    assert_lex_fname "~", :tTILDE
  end

  def test_identifier_defined?
    assert_lex_fname "defined?", :kDEFINED
  end

  def test_index
    assert_lex_fname "[]", :tAREF
  end

  def test_index_equals
    assert_lex_fname "[]=", :tASET
  end

  def test_integer
    assert_scanned "42", :tINTEGER, 42
  end

  def test_integer_bin
    assert_scanned "0b101010", :tINTEGER, 42
  end

  def test_integer_bin_bad_none
    refute_scanned "0b "
  end

  def test_integer_bin_bad_underscores
    refute_scanned "0b10__01"
  end

  def test_integer_dec
    assert_scanned "42", :tINTEGER, 42
  end

  def test_integer_dec_bad_underscores
    refute_scanned "42__24"
  end

  def test_integer_dec_d
    assert_scanned "0d42", :tINTEGER, 42
  end

  def test_integer_dec_d_bad_none
    refute_scanned "0d"
  end

  def test_integer_dec_d_bad_underscores
    refute_scanned "0d42__24"
  end

  def test_question_eh_a__18
    setup_lexer 18

    assert_scanned "?a", :tINTEGER, 97
  end

  def test_question_eh_a__19
    setup_lexer 19

    assert_scanned '?a', :tCHARACTER, "a"
  end

  def test_question_eh_escape_M_escape_C__18
    setup_lexer 18

    assert_scanned '?\M-\C-a', :tINTEGER, 129
  end

  def test_question_eh_escape_M_escape_C__19
    setup_lexer 19

    assert_scanned '?\M-\C-a', :tCHARACTER, "\M-\C-a"
  end

  def test_integer_hex
    assert_scanned "0x2a", :tINTEGER, 42
  end

  def test_integer_hex_bad_none
    refute_scanned "0x "
  end

  def test_integer_hex_bad_underscores
    refute_scanned "0xab__cd"
  end

  def test_integer_oct
    assert_scanned "052", :tINTEGER, 42
  end

  def test_integer_oct_bad_range
    refute_scanned "08"
  end

  def test_integer_oct_bad_underscores
    refute_scanned "01__23"
  end

  def test_integer_oct_O
    assert_scanned "0O52", :tINTEGER, 42
  end

  def test_integer_oct_O_bad_range
    refute_scanned "0O1238"
  end

  def test_integer_oct_O_bad_underscores
    refute_scanned "0O1__23"
  end

  def test_integer_oct_O_not_bad_none
    assert_scanned "0O ", :tINTEGER, 0
  end

  def test_integer_oct_o
    assert_scanned "0o52", :tINTEGER, 42
  end

  def test_integer_oct_o_bad_range
    refute_scanned "0o1283"
  end

  def test_integer_oct_o_bad_underscores
    refute_scanned "0o1__23"
  end

  def test_integer_oct_o_not_bad_none
    assert_scanned "0o ", :tINTEGER, 0
  end

  def test_integer_trailing
    assert_scanned("1.to_s",
                   :tINTEGER, 1,
                   :tDOT, '.',
                   :tIDENTIFIER, 'to_s')
  end

  def test_integer_underscore
    assert_scanned "4_2", :tINTEGER, 42
  end

  def test_integer_underscore_bad
    refute_scanned "4__2"
  end

  def test_integer_zero
    assert_scanned "0", :tINTEGER, 0
  end

  def test_ivar
    assert_scanned "@blah", :tIVAR, "@blah"
  end

  def test_ivar_bad
    refute_scanned "@1"
  end

  def test_ivar_bad_0_length
    refute_scanned "1+@\n", :tINTEGER, 1, :tPLUS, "+"
  end

  def test_keyword_expr
    @lex.state = :expr_endarg

    assert_scanned("if", :kIF_MOD, "if")

    assert_equal :expr_beg, @lex.state
  end

  def test_lt
    assert_scanned "<", :tLT, "<"
  end

  def test_lt2
    assert_scanned("a <\< b",
                   :tIDENTIFIER, "a",
                   :tLSHFT, "<\<",
                   :tIDENTIFIER, "b")

  end

  def test_lt2_equals
    assert_scanned("a <\<= b",
                   :tIDENTIFIER, "a",
                   :tOP_ASGN, "<\<",
                   :tIDENTIFIER, "b")
  end

  def test_lt_equals
    assert_scanned "<=", :tLEQ, "<="
  end

  def test_minus
    assert_scanned("1 - 2",
                   :tINTEGER, 1,
                   :tMINUS, "-",
                   :tINTEGER, 2)
  end

  def test_minus_equals
    @lex.state = :expr_end

    assert_scanned "-=", :tOP_ASGN, "-"
  end

  def test_minus_method
    @lex.state = :expr_fname
    assert_scanned "-", :tMINUS, "-"
  end

  def test_minus_unary_method
    @lex.state = :expr_fname
    assert_scanned "-@", :tUMINUS, "-@"
  end

  def test_minus_unary_number
    assert_scanned("-42",
                   :tUMINUS_NUM, "-",
                   :tINTEGER, 42)
  end

  def test_nth_ref
    assert_scanned('[$1, $2, $3]',
                   :tLBRACK,  "[",
                   :tNTH_REF, 1, :tCOMMA, ",",
                   :tNTH_REF, 2, :tCOMMA, ",",
                   :tNTH_REF, 3,
                   :tRBRACK,  "]")
  end

  def test_open_bracket
    assert_scanned("(", :tLPAREN, "(")
  end

  def test_open_bracket_cmdarg
    assert_scanned("m (", :tIDENTIFIER, "m",
                          :tLPAREN_ARG, "(")
  end

  def test_open_bracket_exprarg
    assert_scanned("m(", :tIDENTIFIER, "m",
                          :tLPAREN2, "(")
  end

  def test_open_curly_bracket
    assert_scanned("{",
                   :tLBRACE, "{")
  end

  def test_open_curly_bracket_arg
    assert_scanned("m { 3 }",
                   :tIDENTIFIER, "m",
                   :tLCURLY, "{",
                   :tINTEGER, 3,
                   :tRCURLY, "}")
  end

  def test_open_curly_bracket_block
    @lex.state = :expr_endarg # seen m(3)

    assert_scanned("{ 4 }",
                   :tLBRACE_ARG, "{",
                   :tINTEGER, 4,
                   :tRCURLY, "}")
  end

  def test_open_square_bracket_arg
    assert_scanned("m [ 3 ]",
                   :tIDENTIFIER, "m",
                   :tLBRACK, "[",
                   :tINTEGER, 3,
                   :tRBRACK, "]")
  end

  def test_open_square_bracket_ary
    assert_scanned("[1, 2, 3]",
                   :tLBRACK, "[",
                   :tINTEGER, 1,
                   :tCOMMA, ",",
                   :tINTEGER, 2,
                   :tCOMMA, ",",
                   :tINTEGER, 3,
                   :tRBRACK, "]")
  end

  def test_open_square_bracket_meth
    assert_scanned("m[3]",
                   :tIDENTIFIER, "m",
                   :tLBRACK2, "[",
                   :tINTEGER, 3,
                   :tRBRACK, "]")
  end

  def test_or
    assert_scanned "|", :tPIPE, "|"
  end

  def test_or2
    assert_scanned "||", :tOROP, "||"
  end

  def test_or2_equals
    assert_scanned "||=", :tOP_ASGN, "||"
  end

  def test_or_equals
    assert_scanned "|=", :tOP_ASGN, "|"
  end

  def test_percent
    assert_scanned("a % 2",
                   :tIDENTIFIER, "a",
                   :tPERCENT, "%",
                   :tINTEGER, 2)
  end

  def test_percent_equals
    assert_scanned("a %= 2",
                   :tIDENTIFIER, "a",
                   :tOP_ASGN, "%",
                   :tINTEGER, 2)
  end

  def test_plus
    assert_scanned("1 + 1",
                   :tINTEGER, 1,
                   :tPLUS, "+",
                   :tINTEGER, 1)
  end

  def test_plus_equals
    @lex.state = :expr_end

    assert_scanned "+=", :tOP_ASGN, "+"
  end

  def test_plus_method
    @lex.state = :expr_fname
    assert_scanned "+", :tPLUS, "+"
  end

  def test_plus_unary_method
    @lex.state = :expr_fname
    assert_scanned "+@", :tUPLUS, "+@"
  end

  def test_numbers
    assert_scanned "0b10", :tINTEGER, 2
    assert_scanned "0B10", :tINTEGER, 2

    assert_scanned "0d10", :tINTEGER, 10
    assert_scanned "0D10", :tINTEGER, 10

    assert_scanned "0x10", :tINTEGER, 16
    assert_scanned "0X10", :tINTEGER, 16

    assert_scanned "0o10", :tINTEGER, 8
    assert_scanned "0O10", :tINTEGER, 8
    assert_scanned "0o",   :tINTEGER, 0
    assert_scanned "0O",   :tINTEGER, 0

    assert_scanned "0o",   :tINTEGER, 0
    assert_scanned "0O",   :tINTEGER, 0

    assert_scanned "0777_333", :tINTEGER, 261851

    assert_scanned "0",    :tINTEGER, 0

    refute_scanned "0x"
    refute_scanned "0X"
    refute_scanned "0b"
    refute_scanned "0B"
    refute_scanned "0d"
    refute_scanned "0D"

    refute_scanned "08"
    refute_scanned "09"
    refute_scanned "0o8"
    refute_scanned "0o9"
    refute_scanned "0O8"
    refute_scanned "0O9"

    refute_scanned "1_e1"
    refute_scanned "1_.1"
    refute_scanned "1__1"

    refute_scanned "1end"
    refute_scanned "1.1end"
  end

  def test_plus_unary_number
    assert_scanned("+42",
                   :tINTEGER, 42)
  end

  def test_question__18
    setup_lexer 18

    assert_scanned "?*", :tINTEGER, 42
  end

  def test_question__19
    setup_lexer 19

    assert_scanned "?*", :tCHARACTER, "*"
  end

  def test_question_bad_eos
    refute_scanned "?"
  end

  def test_question_bad_ws
    assert_scanned "? ",  :tEH, "?"
    assert_scanned "?\n", :tEH, "?"
    assert_scanned "?\t", :tEH, "?"
    assert_scanned "?\v", :tEH, "?"
    assert_scanned "?\r", :tEH, "?"
    assert_scanned "?\f", :tEH, "?"
  end

  def test_question_ws_backslashed__18
    setup_lexer 18

    @lex.state = :expr_beg
    assert_scanned "?\\ ", :tINTEGER, 32
    @lex.state = :expr_beg
    assert_scanned "?\\n", :tINTEGER, 10
    @lex.state = :expr_beg
    assert_scanned "?\\t", :tINTEGER, 9
    @lex.state = :expr_beg
    assert_scanned "?\\v", :tINTEGER, 11
    @lex.state = :expr_beg
    assert_scanned "?\\r", :tINTEGER, 13
    @lex.state = :expr_beg
    assert_scanned "?\\f", :tINTEGER, 12
  end

  def test_question_ws_backslashed__19
    setup_lexer 19

    @lex.state = :expr_beg
    assert_scanned "?\\ ", :tCHARACTER, " "
    @lex.state = :expr_beg
    assert_scanned "?\\n", :tCHARACTER, "\n"
    @lex.state = :expr_beg
    assert_scanned "?\\t", :tCHARACTER, "\t"
    @lex.state = :expr_beg
    assert_scanned "?\\v", :tCHARACTER, "\v"
    @lex.state = :expr_beg
    assert_scanned "?\\r", :tCHARACTER, "\r"
    @lex.state = :expr_beg
    assert_scanned "?\\f", :tCHARACTER, "\f"
  end

  def test_rbracket
    assert_scanned "]", :tRBRACK, "]"
  end

  def test_rcurly
    assert_scanned "}", :tRCURLY, "}"
  end

  def test_regexp
    assert_scanned("/regexp/",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regexp",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_ambiguous
    assert_scanned("method /regexp/",
                   :tIDENTIFIER,     "method",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regexp",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_bad
    refute_scanned("/.*/xyz",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, ".*",
                   :tSTRING_END,     "/")
  end

  def test_regexp_escape_C
    assert_scanned('/regex\\C-x/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\C-x",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_C_M
    assert_scanned('/regex\\C-\\M-x/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\C-\\M-x",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_C_M_craaaazy
    assert_scanned("/regex\\C-\\\n\\M-x/",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\C-\\M-x",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_C_bad_dash
    refute_scanned '/regex\\Cx/', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_C_bad_dash_eos
    refute_scanned '/regex\\C-/', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_C_bad_dash_eos2
    refute_scanned '/regex\\C-', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_C_bad_eos
    refute_scanned '/regex\\C/', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_C_bad_eos2
    refute_scanned '/regex\\c', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_M
    assert_scanned('/regex\\M-x/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\M-x",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_M_C
    assert_scanned('/regex\\M-\\C-x/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\M-\\C-x",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_M_bad_dash
    refute_scanned '/regex\\Mx/', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_M_bad_dash_eos
    refute_scanned '/regex\\M-/', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_M_bad_dash_eos2
    refute_scanned '/regex\\M-', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_M_bad_eos
    refute_scanned '/regex\\M/', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_backslash_slash
    assert_scanned('/\\//',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, '/',
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_backslash_terminator
    assert_scanned('%r%blah\\%blah%',
                   :tREGEXP_BEG,     "%r%",
                   :tSTRING_CONTENT, "blah%blah",
                   :tSTRING_END,     "%",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_backslash_terminator_meta1
    assert_scanned('%r{blah\\}blah}',
                   :tREGEXP_BEG,     "%r{",
                   :tSTRING_CONTENT, "blah}blah",
                   :tSTRING_END,     "}",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_backslash_terminator_meta2
    assert_scanned('%r/blah\\/blah/',
                   :tREGEXP_BEG,     "%r/",
                   :tSTRING_CONTENT, "blah/blah",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_backslash_terminator_meta3
    assert_scanned('%r/blah\\%blah/',
                   :tREGEXP_BEG,     "%r/",
                   :tSTRING_CONTENT, "blah\\%blah",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_bad_eos
    refute_scanned '/regex\\', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_bs
    assert_scanned('/regex\\\\regex/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\\\regex",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_c
    assert_scanned('/regex\\cxxx/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\cxxx",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_c_backslash
    assert_scanned('/regex\\c\\n/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\c\\n",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_chars
    assert_scanned('/re\\tge\\nxp/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "re\\tge\\nxp",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_double_backslash
    assert_scanned('/[\\/\\\\]$/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT,'[/\\\\]$',
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_hex
    assert_scanned('/regex\\x61xp/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\x61xp",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_hex_bad
    refute_scanned '/regex\\xzxp/', :tREGEXP_BEG, "/"
  end

  def test_regexp_escape_hex_one
    assert_scanned('/^[\\xd\\xa]{2}/on',
                   :tREGEXP_BEG,     '/',
                   :tSTRING_CONTENT, '^[\\xd\\xa]{2}',
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     'on')
  end

  def test_regexp_escape_oct1
    assert_scanned('/regex\\0xp/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\0xp",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_oct2
    assert_scanned('/regex\\07xp/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\07xp",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_oct3
    assert_scanned('/regex\\10142/',
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regex\\10142",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_escape_return
    assert_scanned("/regex\\\nregex/",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "regexregex",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_regexp_nm
    assert_scanned("/.*/nm",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, ".*",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "nm")
  end

  def test_rparen
    assert_scanned ")", :tRPAREN, ")"
  end

  def test_rshft
    assert_scanned("a >> 2",
                   :tIDENTIFIER, "a",
                   :tRSHFT, ">>",
                   :tINTEGER, 2)
  end

  def test_rshft_equals
    assert_scanned("a >>= 2",
                   :tIDENTIFIER, "a",
                   :tOP_ASGN, ">>",
                   :tINTEGER, 2)
  end

  def test_star
    assert_scanned("a * ",
                   :tIDENTIFIER, "a",
                   :tSTAR2, "*")

    assert_equal :expr_beg, @lex.state
  end

  def test_star2
    assert_scanned("a ** ",
                   :tIDENTIFIER, "a",
                   :tPOW, "**")

    assert_equal :expr_beg, @lex.state
  end

  def test_star2_equals
    assert_scanned("a **= ",
                   :tIDENTIFIER, "a",
                   :tOP_ASGN, "**")

    assert_equal :expr_beg, @lex.state
  end

  def test_star2_beg
    assert_scanned("** ",
                   :tDSTAR, "**")

    assert_equal :expr_beg, @lex.state
  end

  def test_star_arg
    @lex.state = :expr_arg

    assert_scanned(" *a",
                   :tSTAR, "*",
                   :tIDENTIFIER, "a")

    assert_equal :expr_arg, @lex.state
  end

  def test_star_arg_beg
    @lex.state = :expr_beg

    assert_scanned("*a",
                   :tSTAR, "*",
                   :tIDENTIFIER, "a")

    assert_equal :expr_arg, @lex.state
  end

  def test_star_arg_beg_fname
    @lex.state = :expr_fname

    assert_scanned("*a",
                   :tSTAR2, "*",
                   :tIDENTIFIER, "a")

    assert_equal :expr_arg, @lex.state
  end

  def test_star_equals
    assert_scanned("a *= ",
                   :tIDENTIFIER, "a",
                   :tOP_ASGN, "*")

    assert_equal :expr_beg, @lex.state
  end

  def test_string_bad_eos
    refute_scanned('%',
                   :tSTRING_BEG,     '%')
  end

  def test_string_bad_eos_quote
    refute_scanned('%{nest',
                   :tSTRING_BEG,     '%}')
  end

  def test_string_double
    assert_scanned('"string"',
                   :tSTRING, "string")
  end

  def test_string_double_escape_C
    assert_scanned('"\\C-a"',
                   :tSTRING, "\001")
  end

  def test_string_double_escape_C_backslash
    assert_scanned('"\\C-\\\\"',
                   :tSTRING, "\034")
  end

  def test_string_double_escape_C_escape
    assert_scanned('"\\C-\\M-a"',
                   :tSTRING, "\201")
  end

  def test_string_double_escape_C_question
    assert_scanned('"\\C-?"',
                   :tSTRING, "\177")
  end

  def test_string_double_escape_M
    assert_scanned('"\\M-a"',
                   :tSTRING, "\341")
  end

  def test_string_double_escape_M_backslash
    assert_scanned('"\\M-\\\\"',
                   :tSTRING, "\334")
  end

  def test_string_double_escape_M_escape
    assert_scanned('"\\M-\\C-a"',
                   :tSTRING, "\201")
  end

  def test_string_double_escape_bs1
    assert_scanned('"a\\a\\a"',
                   :tSTRING, "a\a\a")
  end

  def test_string_double_escape_bs2
    assert_scanned('"a\\\\a"',
                   :tSTRING, "a\\a")
  end

  def test_string_double_escape_c
    assert_scanned('"\\ca"',
                   :tSTRING, "\001")
  end

  def test_string_double_escape_c_escape
    assert_scanned('"\\c\\M-a"',
                   :tSTRING, "\201")
  end

  def test_string_double_escape_c_question
    assert_scanned('"\\c?"',
                   :tSTRING, "\177")
  end

  def test_string_double_escape_chars
    assert_scanned('"s\\tri\\ng"',
                   :tSTRING, "s\tri\ng")
  end

  def test_string_double_escape_hex
    assert_scanned('"n = \\x61\\x62\\x63"',
                   :tSTRING, "n = abc")
  end

  def test_string_double_escape_octal
    assert_scanned('"n = \\101\\102\\103"',
                   :tSTRING, "n = ABC")
  end

  def test_string_double_escape_octal_wrap
    assert_scanned('"\\753"',
                   :tSTRING, "\xEB")
  end

  def test_string_double_interp
    assert_scanned("\"blah #x a \#@a b \#$b c \#{3} # \"",
                   :tSTRING_BEG,     "\"",
                   :tSTRING_CONTENT, "blah #x a ",
                   :tSTRING_DVAR,    nil,
                   :tIVAR,           "@a",
                   :tSTRING_CONTENT, " b ",
                   :tSTRING_DVAR,    nil,
                   :tGVAR,           "$b",
                   :tSTRING_CONTENT, " c ",
                   :tSTRING_DBEG,    '#{',
                   :tINTEGER,        3,
                   :tRCURLY,         "}",
                   :tSTRING_CONTENT, " # ",
                   :tSTRING_END,     "\"")
  end

  def test_string_double_interp_label
    assert_scanned('"#{foo:bar}"',
                   :tSTRING_BEG,   '"',
                   :tSTRING_DBEG,  '#{',
                   :tIDENTIFIER,   'foo',
                   :tSYMBOL,       'bar',
                   :tRCURLY,       '}',
                   :tSTRING_END,   '"')
  end

  def test_string_double_nested_curlies
    assert_scanned('%{nest{one{two}one}nest}',
                   :tSTRING_BEG,     '%{',
                   :tSTRING_CONTENT, "nest{one{two}one}nest",
                   :tSTRING_END,     '}')
  end

  def test_string_double_no_interp
    assert_scanned("\"# blah\"",                                # pound first
                   :tSTRING, "# blah")

    assert_scanned("\"blah # blah\"",                           # pound not first
                   :tSTRING, "blah # blah")
  end

  def test_string_escape_x_single
    assert_scanned('"\\x0"',
                   :tSTRING, "\000")
  end

  def test_string_pct_Q
    assert_scanned("%Q[s1 s2]",
                   :tSTRING_BEG,     '%Q[',
                   :tSTRING_CONTENT, "s1 s2",
                   :tSTRING_END,     ']')
  end

  def test_string_pct_W
    assert_scanned("%W[s1 s2\ns3]",
                   :tWORDS_BEG,      "%W[",
                   :tSTRING_CONTENT, "s1",
                   :tSPACE,              nil,
                   :tSTRING_CONTENT, "s2",
                   :tSPACE,              nil,
                   :tSTRING_CONTENT, "s3",
                   :tSPACE,              nil,
                   :tSTRING_END,     ']')
  end

  def test_string_pct_W_bs_nl
    assert_scanned("%W[s1 \\\ns2]",
                   :tWORDS_BEG,      "%W[",
                   :tSTRING_CONTENT, "s1",
                   :tSPACE,              nil,
                   :tSTRING_CONTENT, "\ns2",
                   :tSPACE,              nil,
                   :tSTRING_END,     ']')
  end

  def test_string_pct_W_interp
    assert_scanned('%W[#{1}#{2} #@a]',
                   :tWORDS_BEG,    '%W[',
                   :tSTRING_DBEG,  '#{',
                   :tINTEGER,      1,
                   :tRCURLY,       '}',
                   :tSTRING_DBEG,  '#{',
                   :tINTEGER,      2,
                   :tRCURLY,       '}',
                   :tSPACE,        nil,
                   :tSTRING_DVAR,  nil,
                   :tIVAR,         '@a',
                   :tSPACE,        nil,
                   :tSTRING_END,   ']')
  end

  def test_string_pct_I
    assert_scanned("%I(s1 s2)",
                   :tSYMBOLS_BEG,    "%I(",
                   :tSTRING_CONTENT, "s1",
                   :tSPACE,          nil,
                   :tSTRING_CONTENT, "s2",
                   :tSPACE,          nil,
                   :tSTRING_END,     ')')
  end

  def test_string_pct_angle
    assert_scanned("%<blah>",
                   :tSTRING_BEG,     '%<',
                   :tSTRING_CONTENT, "blah",
                   :tSTRING_END,     '>')
  end

  def test_string_pct_pct
    assert_scanned("%%blah%",
                   :tSTRING_BEG,     '%',
                   :tSTRING_CONTENT, "blah",
                   :tSTRING_END,     '%')
  end

  def test_string_pct_w
    assert_scanned("%w[s1 s2 ]",
                   :tQWORDS_BEG,     "%w[",
                   :tSTRING_CONTENT, "s1",
                   :tSPACE,          nil,
                   :tSTRING_CONTENT, "s2",
                   :tSPACE,          nil,
                   :tSTRING_END,     "]")
  end

  def test_string_pct_w_incomplete
    refute_scanned("%w[s1 ",
                   :tQWORDS_BEG,     "%w[",
                   :tSTRING_CONTENT, "s1",
                   :tSPACE,          nil)
  end

  def test_string_pct_w_bs_nl
    assert_scanned("%w[s1 \\\ns2]",
                   :tQWORDS_BEG,     "%w[",
                   :tSTRING_CONTENT, "s1",
                   :tSPACE,              nil,
                   :tSTRING_CONTENT, "\ns2",
                   :tSPACE,              nil,
                   :tSTRING_END,     ']')
  end

  def test_string_pct_w_bs_sp
    assert_scanned("%w[s\\ 1 s\\ 2]",
                   :tQWORDS_BEG,     "%w[",
                   :tSTRING_CONTENT, "s 1",
                   :tSPACE,              nil,
                   :tSTRING_CONTENT, "s 2",
                   :tSPACE,              nil,
                   :tSTRING_END,     ']')
  end

  def test_string_pct_w_tab
    assert_scanned("%w[abc\tdef]",
                   :tQWORDS_BEG,      "%w[",
                   :tSTRING_CONTENT, "abc",
                   :tSPACE,              nil,
                   :tSTRING_CONTENT, "def",
                   :tSPACE,              nil,
                   :tSTRING_END,     ']')
  end

  def test_string_pct_i
    assert_scanned("%i(s1 s2)",
                   :tQSYMBOLS_BEG,   "%i(",
                   :tSTRING_CONTENT, "s1",
                   :tSPACE,          nil,
                   :tSTRING_CONTENT, "s2",
                   :tSPACE,          nil,
                   :tSTRING_END,     ')')
  end

  def test_string_single
    assert_scanned("'string'",
                   :tSTRING, "string")
  end

  def test_string_single_escape_chars
    assert_scanned("'s\\tri\\ng'",
                   :tSTRING, "s\\tri\\ng")
  end

  def test_string_single_nl
    assert_scanned("'blah\\\nblah'",
                   :tSTRING_BEG,     "'",
                   :tSTRING_CONTENT, "blah\\\n",
                   :tSTRING_CONTENT, "blah",
                   :tSTRING_END,     "'")
  end

  def test_symbol
    assert_scanned(":symbol",
                   :tSYMBOL, "symbol")
  end

  def test_symbol_double
    assert_scanned(":\"symbol\"",
                   :tSYMBEG,         ":\"",
                   :tSTRING_CONTENT, "symbol",
                   :tSTRING_END,     "\"")
  end

  def test_symbol_single
    assert_scanned(":'symbol'",
                   :tSYMBEG,         ":'",
                   :tSTRING_CONTENT, "symbol",
                   :tSTRING_END,     "'")
  end

  def test_ternary
    assert_scanned("a ? b : c",
                   :tIDENTIFIER, "a",
                   :tEH,         "?",
                   :tIDENTIFIER, "b",
                   :tCOLON,      ":",
                   :tIDENTIFIER, "c")

    assert_scanned("a ?b : c",
                   :tIDENTIFIER, "a",
                   :tINTEGER,    98,
                   :tCOLON,      ":",
                   :tIDENTIFIER, "c")

    assert_scanned("a ?bb : c", # GAH! MATZ!!!
                   :tIDENTIFIER, "a",
                   :tEH,         "?",
                   :tIDENTIFIER, "bb",
                   :tCOLON,      ":",
                   :tIDENTIFIER, "c")

    assert_scanned("42 ?", # 42 forces expr_end
                   :tINTEGER,    42,
                   :tEH,         "?")
  end

  def test_tilde
    assert_scanned "~", :tTILDE, "~"
  end

  def test_tilde_unary
    @lex.state = :expr_fname
    assert_scanned "~@", :tTILDE, "~@"
  end

  def test_uminus
    assert_scanned("-blah",
                   :tUMINUS, "-",
                   :tIDENTIFIER, "blah")
  end

  def test_underscore
    assert_scanned("_var", :tIDENTIFIER, "_var")
  end

  def test_underscore_end
    assert_scanned("__END__\n")
    assert_scanned("__END__")
    assert_scanned("__END__ foo",
                   :tIDENTIFIER, '__END__',
                   :tIDENTIFIER, 'foo')
    assert_scanned("__END__\rfoo",
                   :tIDENTIFIER, '__END__',
                   :tIDENTIFIER, 'foo')
  end

  def test_uplus
    assert_scanned("+blah",
                   :tUPLUS, "+",
                   :tIDENTIFIER, "blah")
  end

  def test_if_unless_mod
    assert_scanned("return if true unless false",
                   :kRETURN,      "return",
                   :kIF_MOD,      "if",
                   :kTRUE,        "true",
                   :kUNLESS_MOD,  "unless",
                   :kFALSE,       "false")
  end

  def test_if_stmt
    assert_scanned("if true\n return end",
                   :kIF,          "if",
                   :kTRUE,        "true",
                   :tNL,          nil,
                   :kRETURN,      "return",
                   :kEND,         "end")
  end

  def test_sclass_label
    setup_lexer 20
    assert_scanned("class << a:b",
                   :kCLASS,      'class',
                   :tLSHFT,      '<<',
                   :tIDENTIFIER, 'a',
                   :tSYMBOL,     'b')
  end

  def test_static_env
    env = Parser::StaticEnvironment.new
    env.declare "a"

    @lex.static_env = env
    assert_scanned("a [42]",
                   :tIDENTIFIER, "a",
                   :tLBRACK2,    "[",
                   :tINTEGER,    42,
                   :tRBRACK,     "]")
  end

  def test_int_suffix
    [18, 19, 20].each do |version|
      setup_lexer version

      assert_scanned("42r",
                     :tINTEGER,    42,
                     :tIDENTIFIER, 'r')

      assert_scanned("42if",
                     :tINTEGER,    42,
                     :kIF_MOD,     'if')
    end

    setup_lexer 21

    assert_scanned("42r",  :tRATIONAL,  Rational(42))
    assert_scanned("42i",  :tIMAGINARY, Complex(0, 42))
    assert_scanned("42ri", :tIMAGINARY, Complex(0, Rational(42)))
  end

  def test_float_suffix
    [18, 19, 20].each do |version|
      setup_lexer version

      assert_scanned("42.1r",
                     :tFLOAT,      42.1,
                     :tIDENTIFIER, 'r')

      assert_scanned("42.1if",
                     :tFLOAT,      42.1,
                     :kIF_MOD,     'if')

      assert_scanned("1e1r",
                     :tFLOAT,      1e1,
                     :tIDENTIFIER, 'r')
    end

    begin
      # Feature-check.
      Rational("10")

      setup_lexer 21

      assert_scanned("42.1r",  :tRATIONAL,  Rational(421, 10))
      assert_scanned("42.1i",  :tIMAGINARY, Complex(0, 42.1))
      assert_scanned("42.1ri", :tIMAGINARY, Complex(0, Rational(421, 10)))
      assert_scanned("42.1ir",
                     :tIMAGINARY,  Complex(0, 42.1),
                     :tIDENTIFIER, 'r')

      assert_scanned("1e1i",   :tIMAGINARY, Complex(0, 1e1))
      assert_scanned("1e1r",
                     :tFLOAT,      1e1,
                     :tIDENTIFIER, 'r')
      assert_scanned("1e1ri",
                     :tFLOAT,      1e1,
                     :tIDENTIFIER, 'ri')
      assert_scanned("1e1ir",
                     :tIMAGINARY,  Complex(0, 1e1),
                     :tIDENTIFIER, 'r')
    rescue NoMethodError
      # Ruby not modern enough
    end
  end

  #
  # Tests for whitespace.
  #

  def test_whitespace_fname
    @lex.state = :expr_fname
    assert_scanned('class',
                   :kCLASS, 'class')

    @lex.state = :expr_fname
    assert_scanned(' class',
                   :kCLASS, 'class')

    @lex.state = :expr_fname
    assert_scanned("\nclass",
                   :kCLASS, 'class')

    @lex.state = :expr_fname
    assert_scanned("\\\nclass",
                   :kCLASS, 'class')

    @lex.state = :expr_fname
    assert_scanned("#foo\nclass",
                   :kCLASS, 'class')
  end

  def test_whitespace_endfn
    setup_lexer(21)

    @lex.state = :expr_endfn
    assert_scanned('foo:',
                   :tLABEL, 'foo')

    @lex.state = :expr_endfn
    assert_scanned(' foo:',
                   :tLABEL, 'foo')

    @lex.state = :expr_endfn
    assert_scanned("\nfoo:",
                   :tNL,         nil,
                   :tIDENTIFIER, 'foo',
                   :tCOLON,      ':')

    @lex.state = :expr_endfn
    assert_scanned("\nfoo: ",
                   :tNL,         nil,
                   :tIDENTIFIER, 'foo',
                   :tCOLON,      ':')

    @lex.state = :expr_endfn
    assert_scanned("\\\nfoo:",
                   :tLABEL, 'foo')

    @lex.state = :expr_endfn
    assert_scanned("#foo\nfoo:",
                   :tNL,         nil,
                   :tIDENTIFIER, 'foo',
                   :tCOLON,      ':')

    @lex.state = :expr_endfn
    assert_scanned("#foo\nfoo: ",
                   :tNL,         nil,
                   :tIDENTIFIER, 'foo',
                   :tCOLON,      ':')
  end

  def test_whitespace_dot
    @lex.state = :expr_dot
    assert_scanned('class',
                   :tIDENTIFIER, 'class')

    @lex.state = :expr_dot
    assert_scanned(' class',
                   :tIDENTIFIER, 'class')

    @lex.state = :expr_dot
    assert_scanned("\nclass",
                   :tIDENTIFIER, 'class')

    @lex.state = :expr_dot
    assert_scanned("\\\nclass",
                   :tIDENTIFIER, 'class')

    @lex.state = :expr_dot
    assert_scanned("#foo\nclass",
                   :tIDENTIFIER, 'class')
  end

  def test_whitespace_arg
    @lex.state = :expr_arg
    assert_scanned('+',
                   :tPLUS,  '+')

    @lex.state = :expr_arg
    assert_scanned(' +',
                   :tUPLUS, '+')

    @lex.state = :expr_arg
    assert_scanned("\n+",
                   :tNL,    nil,
                   :tUPLUS, '+')

    @lex.state = :expr_arg
    assert_scanned("\\\n+",
                   :tUPLUS, '+')

    @lex.state = :expr_arg
    assert_scanned("\\\n +",
                   :tUPLUS, '+')

    @lex.state = :expr_arg
    assert_scanned("#foo\n+",
                   :tNL,    nil,
                   :tUPLUS, '+')
  end

  def test_whitespace_endarg
    @lex.state = :expr_endarg
    assert_scanned('{',
                   :tLBRACE_ARG, '{')

    @lex.state = :expr_endarg
    assert_scanned(' {',
                   :tLBRACE_ARG, '{')

    @lex.state = :expr_endarg
    assert_scanned("\n{",
                   :tNL,         nil,
                   :tLBRACE,     '{')

    @lex.state = :expr_endarg
    assert_scanned("\\\n{",
                   :tLBRACE_ARG, '{')

    @lex.state = :expr_endarg
    assert_scanned("#foo\n{",
                   :tNL,         nil,
                   :tLBRACE,     '{')
  end

  def test_whitespace_mid
    @lex.state = :expr_mid
    assert_scanned('+',
                   :tUPLUS, '+')

    @lex.state = :expr_mid
    assert_scanned(' +',
                   :tUPLUS, '+')

    @lex.state = :expr_mid
    assert_scanned("\n+",
                   :tNL,    nil,
                   :tUPLUS, '+')

    @lex.state = :expr_mid
    assert_scanned("\\\n+",
                   :tUPLUS, '+')

    @lex.state = :expr_mid
    assert_scanned("#foo\n+",
                   :tNL,    nil,
                   :tUPLUS, '+')
  end

  def test_whitespace_beg
    @lex.state = :expr_beg
    assert_scanned('+',
                   :tUPLUS, '+')

    @lex.state = :expr_beg
    assert_scanned(' +',
                   :tUPLUS, '+')

    @lex.state = :expr_beg
    assert_scanned("\n+",
                   :tUPLUS, '+')

    @lex.state = :expr_beg
    assert_scanned("\\\n+",
                   :tUPLUS, '+')

    @lex.state = :expr_beg
    assert_scanned("#foo\n+",
                   :tUPLUS, '+')
  end

  def test_whitespace_value
    setup_lexer(20)

    @lex.state = :expr_value
    assert_scanned('a:b',
                   :tIDENTIFIER, 'a',
                   :tSYMBOL,     'b')

    @lex.state = :expr_value
    assert_scanned(' a:b',
                   :tIDENTIFIER, 'a',
                   :tSYMBOL,     'b')

    @lex.state = :expr_value
    assert_scanned("\na:b",
                   :tIDENTIFIER, 'a',
                   :tSYMBOL,     'b')

    @lex.state = :expr_value
    assert_scanned("\\\na:b",
                   :tIDENTIFIER, 'a',
                   :tSYMBOL,     'b')

    @lex.state = :expr_value
    assert_scanned("#foo\na:b",
                   :tIDENTIFIER, 'a',
                   :tSYMBOL,     'b')
  end

  def test_whitespace_end
    @lex.state = :expr_end
    assert_scanned('+ 1',
                   :tPLUS,    '+',
                   :tINTEGER, 1)

    @lex.state = :expr_end
    assert_scanned(' + 1',
                   :tPLUS,    '+',
                   :tINTEGER, 1)

    @lex.state = :expr_end
    assert_scanned("\n+ 1",
                   :tNL,      nil,
                   :tUPLUS,   '+',
                   :tINTEGER, 1)

    @lex.state = :expr_end
    assert_scanned("\\\n+ 1",
                   :tPLUS,    '+',
                   :tINTEGER, 1)

    @lex.state = :expr_end
    assert_scanned("#foo\n+ 1",
                   :tNL,      nil,
                   :tUPLUS,   '+',
                   :tINTEGER, 1)
  end

  def test_whitespace_cr
    setup_lexer(20)
    assert_scanned("<<E\nfoo\nE\rO",
                   :tSTRING_BEG,     '<<"',
                   :tSTRING_CONTENT, "foo\n",
                   :tSTRING_END,     'E',
                   :tNL,             nil)

    setup_lexer(21)
    refute_scanned("<<E\nfoo\nE\rO",
                   :tSTRING_BEG,     '<<"',
                   :tSTRING_CONTENT, "foo\n")
  end

  #
  # Tests for bugs.
  #
  # These tests should be moved from nursery and properly
  # categorized when it's clear how to do that.
  #

  def test_bug_sclass_joined
    assert_scanned("class<<self",
                   :kCLASS, "class",
                   :tLSHFT, "<<",
                   :kSELF,  "self")
  end

  def test_bug_const_expr_end
    assert_scanned("Option",
                   :tCONSTANT, 'Option')

    assert_equal :expr_cmdarg, @lex.state
  end

  def test_bug_expr_beg_div
    @lex.state = :expr_beg
    assert_scanned("/=/",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "=",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")

    @lex.state = :expr_beg
    assert_scanned("/ = /",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, " = ",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_bug_expr_beg_percent
    @lex.state = :expr_beg
    assert_scanned("%=foo=",
                   :tSTRING_BEG,     "%=",
                   :tSTRING_CONTENT, 'foo',
                   :tSTRING_END,     "=")

    @lex.state = :expr_beg
    assert_scanned("% = ",
                   :tSTRING_BEG,     "% ",
                   :tSTRING_CONTENT, '=',
                   :tSTRING_END,     ' ')
  end

  def test_bug_expr_beg_document
    @lex.state = :expr_beg
    assert_scanned(" \n=begin\n=end\nend",
                   :kEND,            "end")

  end

  def test_bug_expr_beg_number
    @lex.state = :expr_beg
    assert_scanned("86400_000_000",
                   :tINTEGER,        86400_000_000)
  end

  def test_bug_expr_beg_backspace_nl
    @lex.state = :expr_beg
    assert_scanned("\n/foo/",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "foo",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")
  end

  def test_bug_expr_beg_heredoc
    assert_scanned("<<EOL % [\nfoo\nEOL\n]",
                   :tSTRING_BEG,      '<<"',
                   :tSTRING_CONTENT,  "foo\n",
                   :tSTRING_END,      'EOL',
                   :tPERCENT,         '%',
                   :tLBRACK,          '[',
                   :tRBRACK,          ']')
  end

  def test_bug_expr_beg_fid
    assert_scanned("Rainbows!",
                   :tFID, 'Rainbows!')
  end

  def test_bug_expr_beg_rescue_assoc
    assert_scanned("rescue=>",
                   :kRESCUE, 'rescue',
                   :tASSOC,  '=>')
  end

  def test_bug_expr_arg_percent
    @lex.state = :expr_arg
    assert_scanned("%[",
                   :tPERCENT, "%",
                   :tLBRACK,  "[")

    @lex.state = :expr_arg
    assert_scanned("%=1",
                   :tOP_ASGN,    "%",
                   :tINTEGER,    1)

    @lex.state = :expr_arg
    assert_scanned(" %[1]",
                   :tSTRING_BEG,     "%[",
                   :tSTRING_CONTENT, '1',
                   :tSTRING_END,     ']')

    @lex.state = :expr_arg
    assert_scanned(" %=1=",
                   :tOP_ASGN,    "%",
                   :tINTEGER,    1,
                   :tEQL,        "=")

    @lex.state = :expr_arg
    assert_scanned(" %\n",
                   :tPERCENT,    '%')
  end

  def test_bug_expr_arg_lt_lt
    @lex.state = :expr_arg
    assert_scanned("<<EOS\nEOS",
                   :tLSHFT,      "<<",
                   :tCONSTANT,   "EOS",
                   :tNL,         nil,
                   :tCONSTANT,   "EOS")

    @lex.state = :expr_arg
    assert_scanned(" <<EOS\nEOS",
                   :tSTRING_BEG, "<<\"",
                   :tSTRING_END, "EOS",
                   :tNL,         nil)
  end

  def test_bug_expr_arg_slash
    @lex.state = :expr_arg
    assert_scanned("/1",
                   :tDIVIDE,    "/",
                   :tINTEGER,   1)

    @lex.state = :expr_arg
    assert_scanned("/ 1",
                   :tDIVIDE,    "/",
                   :tINTEGER,   1)

    @lex.state = :expr_arg
    assert_scanned(" /1/",
                   :tREGEXP_BEG,     "/",
                   :tSTRING_CONTENT, "1",
                   :tSTRING_END,     "/",
                   :tREGEXP_OPT,     "")

    @lex.state = :expr_arg
    assert_scanned(" / 1",
                   :tDIVIDE,    "/",
                   :tINTEGER,   1)
  end

  def test_bug_expr_arg_label
    setup_lexer 19

    @lex.state = :expr_arg
    assert_scanned(" unless:",
                   :tLABEL,     'unless')

    @lex.state = :expr_arg
    assert_scanned(" unless: ",
                   :tLABEL,     'unless')
  end

  def test_bug_heredoc_continuation
    @lex.state = :expr_arg
    assert_scanned(" <<EOS\nEOS\nend",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_END,     "EOS",
                   :tNL,             nil,
                   :kEND,            "end")
  end

  def test_bug_heredoc_cr_lf
    assert_scanned("<<FIN\r\nfoo\r\nFIN\r\n",
                   :tSTRING_BEG,     "<<\"",
                   :tSTRING_CONTENT, "foo\n",
                   :tSTRING_END,     "FIN",
                   :tNL,             nil)
  end

  def test_bug_eh_symbol_no_newline
    assert_scanned("?\"\nfoo",
                   :tINTEGER,     34,
                   :tNL,          nil,
                   :tIDENTIFIER,  "foo")
  end

  def test_bug_expr_arg_newline
    @lex.state = :expr_arg
    assert_scanned("\nfoo",
                   :tNL,          nil,
                   :tIDENTIFIER,  "foo")

    @lex.state = :expr_arg
    assert_scanned(" \nfoo",
                   :tNL,          nil,
                   :tIDENTIFIER,  "foo")

    @lex.state = :expr_arg
    assert_scanned("#foo\nfoo",
                   :tNL,          nil,
                   :tIDENTIFIER,  "foo")
  end

  def test_bug_expr_arg_comment_newline
    @lex.state = :expr_arg
    assert_scanned(" #\nfoo",
                   :tNL,         nil,
                   :tIDENTIFIER, 'foo')
  end

  def test_bug_expr_arg_eh_crlf
    @lex.state = :expr_arg
    assert_scanned(" ?\r\n",
                   :tEH,     '?')
  end

  def test_bug_heredoc_backspace_nl
    assert_scanned(" <<'XXX'\nf \\\nXXX\n",
                   :tSTRING_BEG,     "<<'",
                   :tSTRING_CONTENT, "f \\\n",
                   :tSTRING_END,     "XXX",
                   :tNL,             nil)
  end

  def test_bug_heredoc_lshft
    assert_scanned("<<RULES << CLEANINGS\nRULES",
                   :tSTRING_BEG, '<<"',
                   :tSTRING_END, 'RULES',
                   :tLSHFT,      '<<',
                   :tCONSTANT,   'CLEANINGS')
  end

  def test_bug_sclass_comment_lshft_label
    assert_scanned("class # foo\n<< a:b;end",
                   :kCLASS,      'class',
                   :tLSHFT,      '<<',
                   :tIDENTIFIER, 'a',
                   :tSYMBOL,     'b',
                   :tSEMI,       ';',
                   :kEND,        'end')
  end

  def test_bug_expr_dot_comment
    assert_scanned("foo. #bar\nbaz",
                   :tIDENTIFIER, 'foo',
                   :tDOT,        '.',
                   :tIDENTIFIER, 'baz')
  end

  def test_bug_expr_dot_fid
    assert_scanned("foo.S?",
                   :tIDENTIFIER, 'foo',
                   :tDOT,        '.',
                   :tFID,        'S?')
  end

  def test_bug_expr_dot_id_eq
    assert_scanned("foo.x= 1",
                   :tIDENTIFIER, 'foo',
                   :tDOT,        '.',
                   :tIDENTIFIER, 'x',
                   :tEQL,        '=',
                   :tINTEGER,    1)
  end

  def test_bug_expr_dot_fid_mod
    assert_scanned("foo.x!if 1",
                   :tIDENTIFIER, 'foo',
                   :tDOT,        '.',
                   :tFID,        'x!',
                   :kIF_MOD,     'if',
                   :tINTEGER,    1)
  end

  def test_bug_expr_mid_comment
    assert_scanned("rescue #bar\nprint",
                   :kRESCUE,     'rescue',
                   :tNL,         nil,
                   :tIDENTIFIER, 'print')
  end

  def test_bug_expr_mid_bareword
    assert_scanned("begin; rescue rescue1",
                   :kBEGIN,       'begin',
                   :tSEMI,        ';',
                   :kRESCUE,      'rescue',
                   :tIDENTIFIER,  'rescue1')
  end

  def test_bug_expr_value_document
    assert_scanned("1;\n=begin\n=end",
                   :tINTEGER, 1,
                   :tSEMI,    ';')
  end

  def test_bug_expr_end_colon
    assert_scanned("'foo':'bar'",
                   :tSTRING, 'foo',
                   :tCOLON,  ':',
                   :tSTRING, 'bar')
  end

  def test_bug_expr_value_rescue_colon2
    @lex.state = :expr_value
    assert_scanned("rescue::Exception",
                   :kRESCUE,    'rescue',
                   :tCOLON3,    '::',
                   :tCONSTANT,  'Exception')
  end

  def test_bug_expr_endarg_braces
    assert_scanned("let [] {",
                   :tIDENTIFIER, 'let',
                   :tLBRACK,     '[',
                   :tRBRACK,     ']',
                   :tLBRACE_ARG, '{')
  end

  def test_bug_line_begin_label
    setup_lexer(19)
    assert_scanned("foo:bar",
                   :tIDENTIFIER, 'foo',
                   :tSYMBOL,     'bar')
  end

  def test_bug_interp_expr_value
    assert_scanned('"#{f:a}"',
                   :tSTRING_BEG,  '"',
                   :tSTRING_DBEG, '#{',
                   :tIDENTIFIER,  'f',
                   :tSYMBOL,      'a',
                   :tRCURLY,      '}',
                   :tSTRING_END,  '"')
  end

  def test_bug_const_e
    assert_scanned('E10',
                   :tCONSTANT, 'E10')
    assert_scanned('E4U',
                   :tCONSTANT, 'E4U')
  end

  def test_bug_symbol_newline
    assert_scanned(":foo\n",
                   :tSYMBOL, 'foo',
                   :tNL,     nil)

    assert_scanned(":foo=\n",
                   :tSYMBOL, 'foo=',
                   :tNL,     nil)
  end

  def test_bug_interleaved_heredoc
    assert_scanned(%Q{<<w; "\nfoo\nw\n"},
                   :tSTRING_BEG,     '<<"',
                   :tSTRING_CONTENT, "foo\n",
                   :tSTRING_END,     'w',
                   :tSEMI,           ';',
                   :tSTRING_BEG,     '"',
                   :tSTRING_CONTENT, "\n",
                   :tSTRING_END,     '"')

    @lex.state = :expr_beg
    assert_scanned(%Q{<<w; %w[\nfoo\nw\n1]},
                   :tSTRING_BEG,     '<<"',
                   :tSTRING_CONTENT, "foo\n",
                   :tSTRING_END,     'w',
                   :tSEMI,           ';',
                   :tQWORDS_BEG,     '%w[',
                   :tSTRING_CONTENT, "1",
                   :tSPACE,          nil,
                   :tSTRING_END,     ']')

                   @lex.state = :expr_beg
    assert_scanned(%Q{<<w; "\#{\nfoo\nw\n}"},
                   :tSTRING_BEG,     '<<"',
                   :tSTRING_CONTENT, "foo\n",
                   :tSTRING_END,     'w',
                   :tSEMI,           ';',
                   :tSTRING_BEG,     '"',
                   :tSTRING_DBEG,    '#{',
                   :tRCURLY,         '}',
                   :tSTRING_END,     '"')
  end

  def test_bug_fid_char
    setup_lexer(19)
    assert_scanned(%Q{eof??a},
                   :tFID,       'eof?',
                   :tCHARACTER, 'a')
  end

  def test_bug_nonlabel_context__18
    env = Parser::StaticEnvironment.new
    env.declare "a"

    @lex.static_env = env
    assert_scanned("1+a:a",
                   :tINTEGER,    1,
                   :tPLUS,       '+',
                   :tIDENTIFIER, 'a',
                   :tCOLON,      ':',
                   :tIDENTIFIER, 'a')
  end

  def test_bug_string_percent_newline
    assert_scanned(%Q{%\nfoo\n},
                   :tSTRING_BEG,     "%\n",
                   :tSTRING_CONTENT, 'foo',
                   :tSTRING_END,     "\n")
  end

  def test_bug_string_percent_zero
    assert_scanned(%Q{%\0foo\0},
                   :tSTRING_BEG,     "%\0",
                   :tSTRING_CONTENT, 'foo',
                   :tSTRING_END,     "\0")
  end

  def test_bug_string_utf_escape_composition
    assert_scanned(%q{"\xE2\x80\x99"},
                   :tSTRING, "\xE2\x80\x99")

    if defined?(Encoding)
      assert_scanned(%q{"\xE2\x80\x99"}.force_encoding(Encoding::UTF_8),
                     :tSTRING, '’'.force_encoding(Encoding::UTF_8))
      assert_scanned(%q{"\342\200\231"}.force_encoding(Encoding::UTF_8),
                     :tSTRING, '’'.force_encoding(Encoding::UTF_8))
      assert_scanned(%q{"\M-b\C-\M-@\C-\M-Y"}.force_encoding(Encoding::UTF_8),
                     :tSTRING, '’'.force_encoding(Encoding::UTF_8))
    end
  end

  def test_bug_string_non_utf
    assert_scanned(%Q{"caf\xE9"},
                   :tSTRING, "caf\xE9")
    assert_scanned(%Q{"caf\xC3\xA9"},
                   :tSTRING, "caf\xC3\xA9")

    if defined?(Encoding)
      assert_scanned(%q{"café"}.force_encoding(Encoding::UTF_8),
                     :tSTRING, "café".force_encoding(Encoding::UTF_8))
    end
  end

  def test_bug_semi__END__
    assert_scanned(%Q{foo;\n__END__},
                   :tIDENTIFIER, 'foo',
                   :tSEMI,       ';')
  end

  def test_bug_eql_end
    assert_scanned(%Q{=begin\n#=end\n=end})
  end

  def test_bug_hidden_eof
    @lex.state = :expr_beg
    assert_scanned(%Q{"foo\0\x1a\x04bar"},
                   :tSTRING_BEG,     '"',
                   :tSTRING_CONTENT, "foo\0",
                   :tSTRING_CONTENT, "\x1a",
                   :tSTRING_CONTENT, "\x04",
                   :tSTRING_CONTENT, "bar",
                   :tSTRING_END,     '"')

    @lex.state = :expr_beg
    assert_scanned(%Q{'foo\0\x1a\x04bar'},
                   :tSTRING_BEG,     "'",
                   :tSTRING_CONTENT, "foo\0",
                   :tSTRING_CONTENT, "\x1a",
                   :tSTRING_CONTENT, "\x04",
                   :tSTRING_CONTENT, "bar",
                   :tSTRING_END,     "'")

    @lex.state = :expr_beg
    assert_scanned(%Q{%w[foo\0\x1a\x04bar]},
                   :tQWORDS_BEG,     '%w[',
                   :tSTRING_CONTENT, "foo\0",
                   :tSTRING_CONTENT, "\x1a",
                   :tSTRING_CONTENT, "\x04",
                   :tSTRING_CONTENT, "bar",
                   :tSPACE,          nil,
                   :tSTRING_END,     ']')

    @lex.state = :expr_beg
    assert_scanned(%Q{%W[foo\0\x1a\x04bar]},
                   :tWORDS_BEG,      '%W[',
                   :tSTRING_CONTENT, "foo\0",
                   :tSTRING_CONTENT, "\x1a",
                   :tSTRING_CONTENT, "\x04",
                   :tSTRING_CONTENT, "bar",
                   :tSPACE,          nil,
                   :tSTRING_END,     ']')

    @lex.state = :expr_beg
    assert_scanned(%Q{# foo\0\nbar},
                   :tIDENTIFIER, 'bar')

    @lex.state = :line_begin
    assert_scanned(%Q{=begin\n\0\n=end\nbar},
                   :tIDENTIFIER, 'bar')
  end

  def test_bug_num_adj_kw
    assert_scanned(%q{1if},
                   :tINTEGER, 1,
                   :kIF_MOD,  'if')

    assert_scanned(%q{1.0if},
                   :tFLOAT,   1.0,
                   :kIF_MOD,  'if')
  end

  if defined?(Encoding)
    def test_bug_unicode_in_literal
      setup_lexer(19)
      assert_scanned('"\u00a4"',
                     :tSTRING, "\u00a4")
    end

    def test_bug_utf32le_leak
      setup_lexer(19)
      @lex.force_utf32 = true
      assert_scanned('"F0"',
                     :tSTRING, "F0")
    end
  end

  def test_bug_ragel_stack
    assert_scanned("\"\#{$2 ? $2 : 1}\"",
                   :tSTRING_BEG,      "\"",
                   :tSTRING_DBEG,     "\#{",
                   :tNTH_REF,         2,
                   :tEH,              "?",
                   :tNTH_REF,         2,
                   :tCOLON,           ":",
                   :tINTEGER,         1,
                   :tRCURLY,          "}",
                   :tSTRING_END,      "\"")
  end

end
