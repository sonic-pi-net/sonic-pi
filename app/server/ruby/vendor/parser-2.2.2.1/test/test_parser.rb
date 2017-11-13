# encoding:utf-8

require 'helper'
require 'parse_helper'

class TestParser < Minitest::Test
  include ParseHelper

  def parser_for_ruby_version(version)
    parser = super
    parser.diagnostics.all_errors_are_fatal = true

    %w(foo bar baz).each do |metasyntactic_var|
      parser.static_env.declare(metasyntactic_var)
    end

    parser
  end

  # Guidelines for test naming:
  #  * Test structure follows structure of AST_FORMAT.md.
  #  * Test names follow node names.
  #  * Structurally similar sources may be grouped into one test.
  #  * If, following the guidelines above, names clash, append
  #    an abbreviated disambiguator. E.g. `test_class` and
  #    `test_class_super`.
  #  * When writing a test for a bug, append unabbreviated (but
  #    concise) bug description. E.g. `test_class_bug_missing_newline`.
  #  * Do not append Ruby language version to the name.
  #  * When in doubt, look at existing test names.
  #
  # Guidelines for writing assertions:
  #  * Don't check for structurally same source mapping information
  #    more than once or twice in the entire file. It clutters the
  #    source for no reason.
  #  * Don't forget to check for optional delimiters. `()`, `then`, etc.
  #  * When in doubt, look at existing assertions.

  #
  # Literals
  #

  def test_empty_stmt
    assert_parses(
      nil,
      %q{})
  end

  def test_nil
    assert_parses(
      s(:nil),
      %q{nil},
      %q{~~~ expression})
  end

  def test_nil_expression
    assert_parses(
      s(:begin),
      %q{()},
      %q{^ begin
        | ^ end
        |~~ expression})

    assert_parses(
      s(:kwbegin),
      %q{begin end},
      %q{~~~~~ begin
        |      ~~~ end
        |~~~~~~~~~ expression})
  end

  def test_true
    assert_parses(
      s(:true),
      %q{true},
      %q{~~~~ expression})
  end

  def test_false
    assert_parses(
      s(:false),
      %q{false},
      %q{~~~~~ expression})
  end

  def test_int
    assert_parses(
      s(:int, 42),
      %q{42},
      %q{~~ expression})

    assert_parses(
      s(:int, -42),
      %q{-42},
      %q{^ operator
        |~~~ expression})
  end

  def test_int___LINE__
    assert_parses(
      s(:int, 1),
      %q{__LINE__},
      %q{~~~~~~~~ expression})
  end

  def test_float
    assert_parses(
      s(:float, 1.33),
      %q{1.33},
      %q{~~~~ expression})

    assert_parses(
      s(:float, -1.33),
      %q{-1.33},
      %q{^ operator
        |~~~~~ expression})
  end

  def test_rational
    assert_parses(
      s(:rational, Rational(42)),
      %q{42r},
      %q{~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9 2.0))

    assert_parses(
      s(:rational, Rational(421, 10)),
      %q{42.1r},
      %q{~~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def test_complex
    assert_parses(
      s(:complex, Complex(0, 42)),
      %q{42i},
      %q{~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9 2.0))

    assert_parses(
      s(:complex, Complex(0, Rational(42))),
      %q{42ri},
      %q{~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9 2.0))

    assert_parses(
      s(:complex, Complex(0, 42.1)),
      %q{42.1i},
      %q{~~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9 2.0))

    assert_parses(
      s(:complex, Complex(0, Rational(421, 10))),
      %q{42.1ri},
      %q{~~~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  # Strings

  def test_string_plain
    assert_parses(
      s(:str, 'foobar'),
      %q{'foobar'},
      %q{^ begin
        |       ^ end
        |~~~~~~~~ expression})

    assert_parses(
      s(:str, 'foobar'),
      %q{%q(foobar)},
      %q{^^^ begin
        |         ^ end
        |~~~~~~~~~~ expression})
  end

  def test_string_interp
    assert_parses(
      s(:dstr,
        s(:str, 'foo'),
        s(:begin, s(:lvar, :bar)),
        s(:str, 'baz')),
      %q{"foo#{bar}baz"},
      %q{^ begin
        |             ^ end
        |    ^^ begin (begin)
        |         ^ end (begin)
        |    ~~~~~~ expression (begin)
        |~~~~~~~~~~~~~~ expression})
  end

  def test_string_dvar
    assert_parses(
      s(:dstr,
        s(:ivar, :@a),
        s(:str, ' '),
        s(:cvar, :@@a),
        s(:str, ' '),
        s(:gvar, :$a)),
      %q{"#@a #@@a #$a"})
  end

  def test_string_concat
    assert_parses(
      s(:dstr,
        s(:dstr,
          s(:str, 'foo'),
          s(:ivar, :@a)),
        s(:str, 'bar')),
      %q{"foo#@a" "bar"},
      %q{^ begin (dstr)
        |       ^ end (dstr)
        |         ^ begin (str)
        |             ^ end (str)
        |~~~~~~~~~~~~~~ expression})
  end

  def test_string___FILE__
    assert_parses(
      s(:str, '(assert_parses)'),
      %q{__FILE__},
      %q{~~~~~~~~ expression})
  end

  def test_character
    assert_parses(
      s(:str, 'a'),
      %q{?a},
      %q{^ begin
        |~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:int, 97),
      %q{?a},
      %q{~~ expression},
      %w(1.8))
  end

  def test_heredoc
    assert_parses(
      s(:dstr, s(:str, "foo\n"), s(:str, "bar\n")),
      %Q{<<HERE!foo!bar!HERE}.gsub('!', "\n"),
      %q{~~~~~~ expression
        |       ~~~~~~~~ heredoc_body
        |               ~~~~ heredoc_end})

    assert_parses(
      s(:dstr, s(:str, "foo\n"), s(:str, "bar\n")),
      %Q{<<'HERE'!foo!bar!HERE}.gsub('!', "\n"),
      %q{~~~~~~~~ expression
        |         ~~~~~~~~ heredoc_body
        |                 ~~~~ heredoc_end})

    assert_parses(
      s(:xstr, s(:str, "foo\n"), s(:str, "bar\n")),
      %Q{<<`HERE`!foo!bar!HERE}.gsub('!', "\n"),
      %q{~~~~~~~~ expression
        |         ~~~~~~~~ heredoc_body
        |                 ~~~~ heredoc_end})
  end

  # Symbols

  def test_symbol_plain
    assert_parses(
      s(:sym, :foo),
      %q{:foo},
      %q{~ begin
        |~~~~ expression})

    assert_parses(
      s(:sym, :foo),
      %q{:'foo'},
      %q{^^ begin
        |     ^ end
        |~~~~~~ expression})
  end

  def test_symbol_interp
    assert_parses(
      s(:dsym,
        s(:str, 'foo'),
        s(:begin, s(:lvar, :bar)),
        s(:str, 'baz')),
      %q{:"foo#{bar}baz"},
      %q{^^ begin
        |              ^ end
        |     ^^ begin (begin)
        |          ^ end (begin)
        |     ~~~~~~ expression (begin)
        |~~~~~~~~~~~~~~~ expression})
  end

  def test_symbol_empty
    assert_diagnoses(
      [:error, :empty_symbol],
      %q{:''},
      %q{^^^ location},
      %w(1.8))

    assert_diagnoses(
      [:error, :empty_symbol],
      %q{:""},
      %q{^^^ location},
      %w(1.8))
  end

  # Execute-strings

  def test_xstring_plain
    assert_parses(
      s(:xstr, s(:str, 'foobar')),
      %q{`foobar`},
      %q{^ begin
        |       ^ end
        |~~~~~~~~ expression})
  end

  def test_xstring_interp
    assert_parses(
      s(:xstr,
        s(:str, 'foo'),
        s(:begin, s(:lvar, :bar)),
        s(:str, 'baz')),
      %q{`foo#{bar}baz`},
      %q{^ begin
        |             ^ end
        |    ^^ begin (begin)
        |         ^ end (begin)
        |    ~~~~~~ expression (begin)
        |~~~~~~~~~~~~~~ expression})
  end

  # Regexp

  def test_regex_plain
    assert_parses(
      s(:regexp, s(:str, 'source'), s(:regopt, :i, :m)),
      %q{/source/im},
      %q{^ begin
        |       ^ end
        |        ~~ expression (regopt)
        |~~~~~~~~~~ expression})
  end

  def test_regex_interp
    assert_parses(
      s(:regexp,
        s(:str, 'foo'),
        s(:begin, s(:lvar, :bar)),
        s(:str, 'baz'),
        s(:regopt)),
      %q{/foo#{bar}baz/},
      %q{^ begin
        |    ^^ begin (begin)
        |         ^ end (begin)
        |    ~~~~~~ expression (begin)
        |             ^ end
        |~~~~~~~~~~~~~~ expression})
  end

  def test_regex_error
    # The tests work on 1.8, but with a different message.
    assert_diagnoses(
      [:error, :invalid_regexp, {:message => 'target of repeat operator is not specified: /?/'}],
      %q[/?/],
      %q(~~~ location),
      ALL_VERSIONS - %w(1.8))

    assert_diagnoses(
      [:error, :invalid_regexp, {:message => 'target of repeat operator is not specified: /?/'}],
      %q[/#{""}?/],
      %q(~~~~~~~~ location),
      ALL_VERSIONS - %w(1.8))
  end

  # Arrays

  def test_array_plain
    assert_parses(
      s(:array, s(:int, 1), s(:int, 2)),
      %q{[1, 2]},
      %q{^ begin
        |     ^ end
        |~~~~~~ expression})
  end

  def test_array_splat
    assert_parses(
      s(:array,
        s(:int, 1),
        s(:splat, s(:lvar, :foo)),
        s(:int, 2)),
      %q{[1, *foo, 2]},
      %q{^ begin
        |           ^ end
        |    ^ operator (splat)
        |    ~~~~ expression (splat)
        |~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:array,
        s(:int, 1),
        s(:splat, s(:lvar, :foo))),
      %q{[1, *foo]},
      %q{^ begin
        |        ^ end
        |    ^ operator (splat)
        |    ~~~~ expression (splat)
        |~~~~~~~~~ expression})

    assert_parses(
      s(:array,
        s(:splat, s(:lvar, :foo))),
      %q{[*foo]})
  end

  def test_array_assocs
    assert_parses(
      s(:array,
        s(:hash, s(:pair, s(:int, 1), s(:int, 2)))),
      %q{[ 1 => 2 ]},
      %q{    ~~ operator (hash.pair)
        |  ~~~~~~ expression (hash.pair)
        |  ~~~~~~ expression (hash)})

    assert_parses(
      s(:array,
        s(:int, 1),
        s(:hash, s(:pair, s(:int, 2), s(:int, 3)))),
      %q{[ 1, 2 => 3 ]},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_array_words
    assert_parses(
      s(:array, s(:str, 'foo'), s(:str, 'bar')),
      %q{%w[foo bar]},
      %q{^^^ begin
        |          ^ end
        |   ~~~ expression (str)
        |~~~~~~~~~~~ expression})
  end

  def test_array_words_interp
    assert_parses(
      s(:array,
        s(:str, 'foo'),
        s(:dstr, s(:begin, s(:lvar, :bar)))),
      %q{%W[foo #{bar}]},
      %q{^^^ begin
        |       ^^ begin (dstr.begin)
        |            ^ end (dstr.begin)
        |       ~~~~~~ expression (dstr.begin)
        |             ^ end
        |   ~~~ expression (str)
        |         ~~~ expression (dstr.begin.lvar)
        |~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:array,
        s(:str, 'foo'),
        s(:dstr,
          s(:begin, s(:lvar, :bar)),
          s(:str, 'foo'),
          s(:ivar, :@baz))),
      %q{%W[foo #{bar}foo#@baz]})
  end

  def test_array_words_empty
    assert_parses(
      s(:array),
      %q{%w[]},
      %q{^^^ begin
        |   ^ end
        |~~~~ expression})

    assert_parses(
      s(:array),
      %q{%W()})
  end

  def test_array_symbols
    assert_parses(
      s(:array, s(:sym, :foo), s(:sym, :bar)),
      %q{%i[foo bar]},
      %q{^^^ begin
        |          ^ end
        |   ~~~ expression (sym)
        |~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_array_symbols_interp
    assert_parses(
      s(:array,
        s(:sym, :foo),
        s(:dsym, s(:begin, s(:lvar, :bar)))),
      %q{%I[foo #{bar}]},
      %q{^^^ begin
        |             ^ end
        |   ~~~ expression (sym)
        |       ^^ begin (dsym.begin)
        |            ^ end (dsym.begin)
        |       ~~~~~~ expression (dsym.begin)
        |         ~~~ expression (dsym.begin.lvar)
        |~~~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses(
      s(:array,
        s(:dsym,
          s(:str, 'foo'),
          s(:begin, s(:lvar, :bar)))),
      %q{%I[foo#{bar}]},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_array_symbols_empty
    assert_parses(
      s(:array),
      %q{%i[]},
      %q{^^^ begin
        |   ^ end
        |~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses(
      s(:array),
      %q{%I()},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  # Hashes

  def test_hash_empty
    assert_parses(
      s(:hash),
      %q[{ }],
      %q{^ begin
        |  ^ end
        |~~~ expression})
  end

  def test_hash_hashrocket
    assert_parses(
      s(:hash, s(:pair, s(:int, 1), s(:int, 2))),
      %q[{ 1 => 2 }],
      %q{^ begin
        |         ^ end
        |    ^^ operator (pair)
        |  ~~~~~~ expression (pair)
        |~~~~~~~~~~ expression})

    assert_parses(
      s(:hash,
        s(:pair, s(:int, 1), s(:int, 2)),
        s(:pair, s(:sym, :foo), s(:str, 'bar'))),
      %q[{ 1 => 2, :foo => "bar" }])
  end

  def test_hash_label
    assert_parses(
      s(:hash, s(:pair, s(:sym, :foo), s(:int, 2))),
      %q[{ foo: 2 }],
      %q{^ begin
        |         ^ end
        |     ^ operator (pair)
        |  ~~~ expression (pair.sym)
        |  ~~~~~~ expression (pair)
        |~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_hash_label_end
    assert_parses(
      s(:hash, s(:pair, s(:sym, :foo), s(:int, 2))),
      %q[{ 'foo': 2 }],
      %q{^ begin
        |           ^ end
        |       ^ operator (pair)
        |  ^ begin (pair.sym)
        |      ^ end (pair.sym)
        |  ~~~~~ expression (pair.sym)
        |  ~~~~~~~~ expression (pair)
        |~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8 1.9 2.0 2.1))

    assert_parses(
      s(:hash,
        s(:pair, s(:sym, :foo), s(:int, 2)),
        s(:pair, s(:sym, :bar), s(:hash))),
      %q[{ 'foo': 2, 'bar': {}}],
      %q{},
      ALL_VERSIONS - %w(1.8 1.9 2.0 2.1))

    assert_parses(
      s(:send, nil, :f,
        s(:if, s(:send, nil, :a),
          s(:str, "a"),
          s(:int, 1))),
      %q{f(a ? "a":1)},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9 2.0 2.1))
  end

  def test_hash_kwsplat
    assert_parses(
      s(:hash,
        s(:pair, s(:sym, :foo), s(:int, 2)),
        s(:kwsplat, s(:lvar, :bar))),
      %q[{ foo: 2, **bar }],
      %q{          ^^ operator (kwsplat)
        |          ~~~~~ expression (kwsplat)},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_hash_no_hashrocket
    assert_parses(
      s(:hash, s(:pair, s(:int, 1), s(:int, 2))),
      %q[{ 1, 2 }],
      %q{^ begin
        |       ^ end
        |  ~~~~ expression (pair)
        |~~~~~~~~ expression},
      %w(1.8))
  end

  def test_hash_no_hashrocket_odd
    assert_diagnoses(
      [:error, :odd_hash],
      %q[{ 1, 2, 3 }],
      %q(        ~ location),
      %w(1.8))
  end

  # Range

  def test_range_inclusive
    assert_parses(
      s(:irange, s(:int, 1), s(:int, 2)),
      %q{1..2},
      %q{ ~~ operator
        |~~~~ expression})
  end

  def test_range_exclusive
    assert_parses(
      s(:erange, s(:int, 1), s(:int, 2)),
      %q{1...2},
      %q{ ~~~ operator
        |~~~~~ expression})
  end

  #
  # Access
  #

  # Variables and pseudovariables

  def test_self
    assert_parses(
      s(:self),
      %q{self},
      %q{~~~~ expression})
  end

  def test_lvar
    assert_parses(
      s(:lvar, :foo),
      %q{foo},
      %q{~~~ expression})
  end

  def test_ivar
    assert_parses(
      s(:ivar, :@foo),
      %q{@foo},
      %q{~~~~ expression})
  end

  def test_cvar
    assert_parses(
      s(:cvar, :@@foo),
      %q{@@foo},
      %q{~~~~~ expression})
  end

  def test_gvar
    assert_parses(
      s(:gvar, :$foo),
      %q{$foo},
      %q{~~~~ expression})
  end

  def test_gvar_dash_empty
    assert_diagnoses(
      [:fatal, :unexpected, { :character => '$' }],
      %q{$- },
      %q{^ location},
      %w(2.1))
  end

  def test_back_ref
    assert_parses(
      s(:back_ref, :$+),
      %q{$+},
      %q{~~ expression})
  end

  def test_nth_ref
    assert_parses(
      s(:nth_ref, 10),
      %q{$10},
      %q{~~~ expression})
  end

  # Constants

  def test_const_toplevel
    assert_parses(
      s(:const, s(:cbase), :Foo),
      %q{::Foo},
      %q{  ~~~ name
        |~~ double_colon
        |~~~~~ expression})
  end

  def test_const_scoped
    assert_parses(
      s(:const, s(:const, nil, :Bar), :Foo),
      %q{Bar::Foo},
      %q{     ~~~ name
        |   ~~ double_colon
        |~~~~~~~~ expression})
  end

  def test_const_unscoped
    assert_parses(
      s(:const, nil, :Foo),
      %q{Foo},
      %q{~~~ name
        |~~~ expression})
  end

  def test___ENCODING__
    assert_parses(
      s(:const, s(:const, nil, :Encoding), :UTF_8),
      %q{__ENCODING__},
      %q{~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  # defined?

  def test_defined
    assert_parses(
      s(:defined?, s(:lvar, :foo)),
      %q{defined? foo},
      %q{~~~~~~~~ keyword
        |~~~~~~~~~~~~ expression})

    assert_parses(
      s(:defined?, s(:lvar, :foo)),
      %q{defined?(foo)},
      %q{~~~~~~~~ keyword
        |        ^ begin
        |            ^ end
        |~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:defined?, s(:ivar, :@foo)),
      %q{defined? @foo})
  end

  #
  # Assignment
  #

  # Variables

  def test_lvasgn
    assert_parses(
      s(:begin,
        s(:lvasgn, :var, s(:int, 10)),
        s(:lvar, :var)),
      %q{var = 10; var},
      %q{~~~ name (lvasgn)
        |    ^ operator (lvasgn)
        |~~~~~~~~ expression (lvasgn)
        })
  end

  def test_ivasgn
    assert_parses(
      s(:ivasgn, :@var, s(:int, 10)),
      %q{@var = 10},
      %q{~~~~ name
        |     ^ operator
        |~~~~~~~~~ expression
        })
  end

  def test_cvasgn
    assert_parses(
      s(:cvasgn, :@@var, s(:int, 10)),
      %q{@@var = 10},
      %q{~~~~~ name
        |      ^ operator
        |~~~~~~~~~~ expression
        })
  end

  def test_gvasgn
    assert_parses(
      s(:gvasgn, :$var, s(:int, 10)),
      %q{$var = 10},
      %q{~~~~ name
        |     ^ operator
        |~~~~~~~~~ expression
        })
  end

  def test_asgn_cmd
    assert_parses(
      s(:lvasgn, :foo, s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo = m foo})

    assert_parses(
      s(:lvasgn, :foo,
        s(:lvasgn, :bar,
          s(:send, nil, :m, s(:lvar, :foo)))),
      %q{foo = bar = m foo},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_asgn_keyword_invalid
    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{nil = foo},
      %q{~~~ location})

    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{self = foo},
      %q{~~~~ location})

    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{true = foo},
      %q{~~~~ location})

    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{false = foo},
      %q{~~~~~ location})

    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{__FILE__ = foo},
      %q{~~~~~~~~ location})

    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{__LINE__ = foo},
      %q{~~~~~~~~ location})
  end

  def test_asgn_backref_invalid
    assert_diagnoses(
      [:error, :backref_assignment],
      %q{$1 = foo},
      %q{~~ location})
  end

  # Constants

  def test_casgn_toplevel
    assert_parses(
      s(:casgn, s(:cbase), :Foo, s(:int, 10)),
      %q{::Foo = 10},
      %q{  ~~~ name
        |      ^ operator
        |~~ double_colon
        |~~~~~~~~~~ expression
        })
  end

  def test_casgn_scoped
    assert_parses(
      s(:casgn, s(:const, nil, :Bar), :Foo, s(:int, 10)),
      %q{Bar::Foo = 10},
      %q{     ~~~ name
        |         ^ operator
        |   ~~ double_colon
        |~~~~~~~~~~~~~ expression
        })
  end

  def test_casgn_unscoped
    assert_parses(
      s(:casgn, nil, :Foo, s(:int, 10)),
      %q{Foo = 10},
      %q{~~~ name
        |    ^ operator
        |~~~~~~~~ expression
        })
  end

  def test_casgn_invalid
    assert_diagnoses(
      [:error, :dynamic_const],
      %q{def f; Foo = 1; end},
      %q{       ~~~ location})

    assert_diagnoses(
      [:error, :dynamic_const],
      %q{def f; Foo::Bar = 1; end},
      %q{       ~~~~~~~~ location})

    assert_diagnoses(
      [:error, :dynamic_const],
      %q{def f; ::Bar = 1; end},
      %q{       ~~~~~ location})
  end

  # Multiple assignment

  def test_masgn
    assert_parses(
      s(:masgn,
        s(:mlhs, s(:lvasgn, :foo), s(:lvasgn, :bar)),
        s(:array, s(:int, 1), s(:int, 2))),
      %q{foo, bar = 1, 2},
      %q{         ^ operator
        |~~~~~~~~ expression (mlhs)
        |           ~~~~ expression (array)
        |~~~~~~~~~~~~~~~ expression
        })

    assert_parses(
      s(:masgn,
        s(:mlhs, s(:lvasgn, :foo), s(:lvasgn, :bar)),
        s(:array, s(:int, 1), s(:int, 2))),
      %q{(foo, bar) = 1, 2},
      %q{^ begin (mlhs)
        |         ^ end (mlhs)
        |~~~~~~~~~~ expression (mlhs)
        |~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:lvasgn, :foo),
          s(:lvasgn, :bar),
          s(:lvasgn, :baz)),
        s(:array, s(:int, 1), s(:int, 2))),
      %q{foo, bar, baz = 1, 2})
  end

  def test_masgn_splat
    assert_parses(
      s(:masgn,
        s(:mlhs, s(:ivasgn, :@foo), s(:cvasgn, :@@bar)),
        s(:array, s(:splat, s(:lvar, :foo)))),
      %q{@foo, @@bar = *foo},
      %q{              ^ operator (array.splat)
        |              ~~~~ expression (array.splat)
        })

    assert_parses(
      s(:masgn,
        s(:mlhs, s(:lvasgn, :a), s(:lvasgn, :b)),
        s(:array, s(:splat, s(:lvar, :foo)), s(:lvar, :bar))),
      %q{a, b = *foo, bar},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:masgn,
        s(:mlhs, s(:lvasgn, :a), s(:splat, s(:lvasgn, :b))),
        s(:lvar, :bar)),
      %q{a, *b = bar})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:lvasgn, :a),
          s(:splat, s(:lvasgn, :b)),
          s(:lvasgn, :c)),
        s(:lvar, :bar)),
      %q{a, *b, c = bar},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:masgn,
        s(:mlhs, s(:lvasgn, :a), s(:splat)),
        s(:lvar, :bar)),
      %q{a, * = bar})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:lvasgn, :a),
          s(:splat),
          s(:lvasgn, :c)),
        s(:lvar, :bar)),
      %q{a, *, c = bar},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:masgn,
        s(:mlhs, s(:splat, s(:lvasgn, :b))),
        s(:lvar, :bar)),
      %q{*b = bar})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:splat, s(:lvasgn, :b)),
          s(:lvasgn, :c)),
        s(:lvar, :bar)),
      %q{*b, c = bar},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:masgn,
        s(:mlhs, s(:splat)),
        s(:lvar, :bar)),
      %q{* = bar})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:splat),
          s(:lvasgn, :c),
          s(:lvasgn, :d)),
        s(:lvar, :bar)),
      %q{*, c, d = bar},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_masgn_nested
    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:lvasgn, :a),
          s(:mlhs,
            s(:lvasgn, :b),
            s(:lvasgn, :c))),
        s(:lvar, :foo)),
      %q{a, (b, c) = foo},
      %q{   ^ begin (mlhs.mlhs)
        |        ^ end (mlhs.mlhs)
        |   ~~~~~~ expression (mlhs.mlhs)
        })

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:lvasgn, :b)),
        s(:lvar, :foo)),
      %q{((b, )) = foo},
      %q{^ begin (mlhs)
        |      ^ end (mlhs)})
  end

  def test_masgn_attr
    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:send, s(:self), :a=),
          s(:send, s(:self), :[]=, s(:int, 1), s(:int, 2))),
        s(:lvar, :foo)),
      %q{self.a, self[1, 2] = foo},
      %q{~~~~~~ expression (mlhs.send/1)
        |     ~ selector (mlhs.send/1)
        |            ~~~~~~ selector (mlhs.send/2)
        |        ~~~~~~~~~~ expression (mlhs.send/2)})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:send, s(:self), :a=),
          s(:lvasgn, :foo)),
        s(:lvar, :foo)),
      %q{self::a, foo = foo})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:send, s(:self), :A=),
          s(:lvasgn, :foo)),
        s(:lvar, :foo)),
      %q{self.A, foo = foo})
  end

  def test_masgn_const
    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:casgn, s(:self), :A),
          s(:lvasgn, :foo)),
        s(:lvar, :foo)),
      %q{self::A, foo = foo})

    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:casgn, s(:cbase), :A),
          s(:lvasgn, :foo)),
        s(:lvar, :foo)),
      %q{::A, foo = foo})
  end

  def test_masgn_cmd
    assert_parses(
      s(:masgn,
        s(:mlhs,
          s(:lvasgn, :foo),
          s(:lvasgn, :bar)),
        s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo, bar = m foo})
  end

  def test_asgn_mrhs
    assert_parses(
      s(:lvasgn, :foo,
        s(:array, s(:lvar, :bar), s(:int, 1))),
      %q{foo = bar, 1},
      %q{      ~~~~~~ expression (array)
        |~~~~~~~~~~~~ expression})

    assert_parses(
      s(:lvasgn, :foo,
        s(:array, s(:splat, s(:lvar, :bar)))),
      %q{foo = *bar})

    assert_parses(
      s(:lvasgn, :foo,
        s(:array,
          s(:lvar, :baz),
          s(:splat, s(:lvar, :bar)))),
      %q{foo = baz, *bar})
  end

  def test_masgn_keyword_invalid
    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{nil, foo = bar},
      %q{~~~ location})
  end

  def test_masgn_backref_invalid
    assert_diagnoses(
      [:error, :backref_assignment],
      %q{$1, = foo},
      %q{~~ location})
  end

  def test_masgn_const_invalid
    assert_diagnoses(
      [:error, :dynamic_const],
      %q{def f; self::A, foo = foo; end},
      %q{       ~~~~~~~ location})

    assert_diagnoses(
      [:error, :dynamic_const],
      %q{def f; ::A, foo = foo; end},
      %q{       ~~~ location})
  end

  # Variable binary operator-assignment

  def test_var_op_asgn
    assert_parses(
      s(:op_asgn, s(:lvasgn, :a), :+, s(:int, 1)),
      %q{a += 1},
      %q{  ^^ operator
        |~~~~~~ expression})

    assert_parses(
      s(:op_asgn, s(:ivasgn, :@a), :|, s(:int, 1)),
      %q{@a |= 1},
      %q{   ^^ operator
        |~~~~~~~ expression})

    assert_parses(
      s(:op_asgn, s(:cvasgn, :@@var), :|, s(:int, 10)),
      %q{@@var |= 10})

    assert_parses(
      s(:def, :a, s(:args),
        s(:op_asgn, s(:cvasgn, :@@var), :|, s(:int, 10))),
      %q{def a; @@var |= 10; end})
  end

  def test_var_op_asgn_cmd
    assert_parses(
      s(:op_asgn,
        s(:lvasgn, :foo), :+,
        s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo += m foo})
  end

  def test_var_op_asgn_keyword_invalid
    assert_diagnoses(
      [:error, :invalid_assignment],
      %q{nil += foo},
      %q{~~~ location})
  end

  def test_const_op_asgn
    assert_parses(
      s(:op_asgn,
        s(:casgn, nil, :A), :+,
        s(:int, 1)),
      %q{A += 1})

    assert_parses(
      s(:op_asgn,
        s(:casgn, s(:cbase), :A), :+,
        s(:int, 1)),
      %q{::A += 1},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses(
      s(:op_asgn,
        s(:casgn, s(:const, nil, :B), :A), :+,
        s(:int, 1)),
      %q{B::A += 1},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses(
      s(:def, :x, s(:args),
        s(:or_asgn,
          s(:casgn, s(:self), :A),
          s(:int, 1))),
      %q{def x; self::A ||= 1; end},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses(
      s(:def, :x, s(:args),
        s(:or_asgn,
          s(:casgn, s(:cbase), :A),
          s(:int, 1))),
      %q{def x; ::A ||= 1; end},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_const_op_asgn_invalid
    assert_diagnoses(
      [:error, :dynamic_const],
      %q{Foo::Bar += 1},
      %q{     ~~~ location},
      %w(1.8 1.9))

    assert_diagnoses(
      [:error, :dynamic_const],
      %q{::Bar += 1},
      %q{  ~~~ location},
      %w(1.8 1.9))

    assert_diagnoses(
      [:error, :dynamic_const],
      %q{def foo; Foo::Bar += 1; end},
      %q{              ~~~ location},
      %w(1.8 1.9))

    assert_diagnoses(
      [:error, :dynamic_const],
      %q{def foo; ::Bar += 1; end},
      %q{           ~~~ location},
      %w(1.8 1.9))
  end

  # Method binary operator-assignment

  def test_op_asgn
    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :a), :+,
        s(:int, 1)),
      %q{foo.a += 1},
      %q{      ^^ operator
        |    ~ selector (send)
        |~~~~~ expression (send)
        |~~~~~~~~~~ expression})

    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :a), :+,
        s(:int, 1)),
      %q{foo::a += 1})

    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :A), :+,
        s(:int, 1)),
      %q{foo.A += 1})
  end

  def test_op_asgn_cmd
    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :a), :+,
        s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo.a += m foo})

    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :a), :+,
        s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo::a += m foo})

    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :A), :+,
        s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo.A += m foo})

    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :A), :+,
        s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo::A += m foo},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_op_asgn_index
    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :[],
          s(:int, 0), s(:int, 1)), :+,
        s(:int, 2)),
      %q{foo[0, 1] += 2},
      %q{          ^^ operator
        |   ~~~~~~ selector (send)
        |~~~~~~~~~ expression (send)
        |~~~~~~~~~~~~~~ expression})
  end

  def test_op_asgn_index_cmd
    assert_parses(
      s(:op_asgn,
        s(:send, s(:lvar, :foo), :[],
          s(:int, 0), s(:int, 1)), :+,
        s(:send, nil, :m, s(:lvar, :foo))),
      %q{foo[0, 1] += m foo})
  end

  def test_op_asgn_invalid
    assert_diagnoses(
      [:error, :backref_assignment],
      %q{$1 |= 1},
      %q{~~ location})

    assert_diagnoses(
      [:error, :backref_assignment],
      %q{$+ |= 1},
      %q{~~ location})

    assert_diagnoses(
      [:error, :backref_assignment],
      %q{$+ |= m foo},
      %q{~~ location})
  end

  # Variable logical operator-assignment

  def test_var_or_asgn
    assert_parses(
      s(:or_asgn, s(:lvasgn, :a), s(:int, 1)),
      %q{a ||= 1},
      %q{  ^^^ operator
        |~~~~~~~ expression})
  end

  def test_var_and_asgn
    assert_parses(
      s(:and_asgn, s(:lvasgn, :a), s(:int, 1)),
      %q{a &&= 1},
      %q{  ^^^ operator
        |~~~~~~~ expression})
  end

  # Method logical operator-assignment

  def test_or_asgn
    assert_parses(
      s(:or_asgn,
        s(:send, s(:lvar, :foo), :a),
        s(:int, 1)),
      %q{foo.a ||= 1},
      %q{      ^^^ operator
        |    ~ selector (send)
        |~~~~~ expression (send)
        |~~~~~~~~~~~ expression})

    assert_parses(
      s(:or_asgn,
        s(:send, s(:lvar, :foo), :[],
          s(:int, 0), s(:int, 1)),
        s(:int, 2)),
      %q{foo[0, 1] ||= 2},
      %q{          ^^^ operator
        |   ~~~~~~ selector (send)
        |~~~~~~~~~ expression (send)
        |~~~~~~~~~~~~~~~ expression})
  end

  def test_and_asgn
    assert_parses(
      s(:and_asgn,
        s(:send, s(:lvar, :foo), :a),
        s(:int, 1)),
      %q{foo.a &&= 1},
      %q{      ^^^ operator
        |    ~ selector (send)
        |~~~~~ expression (send)
        |~~~~~~~~~~~ expression})

    assert_parses(
      s(:and_asgn,
        s(:send, s(:lvar, :foo), :[],
          s(:int, 0), s(:int, 1)),
        s(:int, 2)),
      %q{foo[0, 1] &&= 2},
      %q{          ^^^ operator
        |   ~~~~~~ selector (send)
        |~~~~~~~~~ expression (send)
        |~~~~~~~~~~~~~~~ expression})
  end

  def test_log_asgn_invalid
    assert_diagnoses(
      [:error, :backref_assignment],
      %q{$1 &&= 1},
      %q{~~ location})

    assert_diagnoses(
      [:error, :backref_assignment],
      %q{$+ ||= 1},
      %q{~~ location})
  end


  #
  # Class and module definitions
  #

  def test_module
    assert_parses(
      s(:module,
        s(:const, nil, :Foo),
        nil),
      %q{module Foo; end},
      %q{~~~~~~ keyword
        |       ~~~ name
        |            ~~~ end})
  end

  def test_module_invalid
    assert_diagnoses(
      [:error, :module_in_def],
      %q{def a; module Foo; end; end},
      %q{       ^^^^^^ location})
  end

  def test_cpath
    assert_parses(
      s(:module,
        s(:const, s(:cbase), :Foo),
        nil),
      %q{module ::Foo; end})

    assert_parses(
      s(:module,
        s(:const, s(:const, nil, :Bar), :Foo),
        nil),
      %q{module Bar::Foo; end})
  end

  def test_cpath_invalid
    assert_diagnoses(
      [:error, :module_name_const],
      %q{module foo; end})
  end

  def test_class
    assert_parses(
      s(:class,
        s(:const, nil, :Foo),
        nil,
        nil),
      %q{class Foo; end},
      %q{~~~~~ keyword
        |      ~~~ name
        |           ~~~ end})
  end

  def test_class_super
    assert_parses(
      s(:class,
        s(:const, nil, :Foo),
        s(:const, nil, :Bar),
        nil),
      %q{class Foo < Bar; end},
      %q{~~~~~ keyword
        |          ^ operator
        |                 ~~~ end})
  end

  def test_class_super_label
    assert_parses(
      s(:class,
        s(:const, nil, :Foo),
        s(:send, nil, :a,
          s(:sym, :b)),
        nil),
      %q{class Foo < a:b; end},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_class_invalid
    assert_diagnoses(
      [:error, :class_in_def],
      %q{def a; class Foo; end; end},
      %q{       ^^^^^ location})
  end

  def test_sclass
    assert_parses(
      s(:sclass,
        s(:lvar, :foo),
        s(:nil)),
      %q{class << foo; nil; end},
      %q{~~~~~ keyword
        |      ^^ operator
        |                   ~~~ end})
  end

  #
  # Method (un)definition
  #

  def test_def
    assert_parses(
      s(:def, :foo, s(:args), nil),
      %q{def foo; end},
      %q{~~~ keyword
        |    ~~~ name
        |         ~~~ end})

    assert_parses(
      s(:def, :String, s(:args), nil),
      %q{def String; end})

    assert_parses(
      s(:def, :String=, s(:args), nil),
      %q{def String=; end})

    assert_parses(
      s(:def, :until, s(:args), nil),
      %q{def until; end})
  end

  def test_defs
    assert_parses(
      s(:defs, s(:self), :foo, s(:args), nil),
      %q{def self.foo; end},
      %q{~~~ keyword
        |        ^ operator
        |         ~~~ name
        |              ~~~ end})

    assert_parses(
      s(:defs, s(:self), :foo, s(:args), nil),
      %q{def self::foo; end},
      %q{~~~ keyword
        |        ^^ operator
        |          ~~~ name
        |               ~~~ end})

    assert_parses(
      s(:defs, s(:lvar, :foo), :foo, s(:args), nil),
      %q{def (foo).foo; end})

    assert_parses(
      s(:defs, s(:const, nil, :String), :foo,
        s(:args), nil),
      %q{def String.foo; end})

    assert_parses(
      s(:defs, s(:const, nil, :String), :foo,
        s(:args), nil),
      %q{def String::foo; end})
  end

  def test_defs_invalid
    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def (1).foo; end},
      %q{     ~ location})

    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def ("foo").foo; end},
      %q{     ~~~~~ location})

    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def ("foo#{bar}").foo; end},
      %q{     ~~~~~~~~~~~ location})

    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def (:foo).foo; end},
      %q{     ~~~~ location})

    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def (:"foo#{bar}").foo; end},
      %q{     ~~~~~~~~~~~~ location})

    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def ([]).foo; end},
      %q{     ~~ location})

    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def ({}).foo; end},
      %q{     ~~ location})

    assert_diagnoses(
      [:error, :singleton_literal],
      %q{def (/foo/).foo; end},
      %q{     ~~~~~ location})
  end

  def test_undef
    assert_parses(
      s(:undef,
        s(:sym, :foo),
        s(:sym, :bar),
        s(:dsym, s(:str, 'foo'), s(:begin, s(:int, 1)))),
      %q{undef foo, :bar, :"foo#{1}"},
      %q{~~~~~ keyword
        |      ~~~ expression (sym/1)
        |           ~~~~ expression (sym/2)
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  #
  # Aliasing
  #

  def test_alias
    assert_parses(
      s(:alias, s(:sym, :foo), s(:sym, :bar)),
      %q{alias :foo bar},
      %q{~~~~~ keyword
        |      ~~~~ expression (sym/1)
        |           ~~~ expression (sym/2)
        |~~~~~~~~~~~~~~ expression})
  end

  def test_alias_gvar
    assert_parses(
      s(:alias, s(:gvar, :$a), s(:gvar, :$b)),
      %q{alias $a $b},
      %q{      ~~ expression (gvar/1)})

    assert_parses(
      s(:alias, s(:gvar, :$a), s(:back_ref, :$+)),
      %q{alias $a $+},
      %q{         ~~ expression (back_ref)})
  end

  def test_alias_nth_ref
    assert_diagnoses(
      [:error, :nth_ref_alias],
      %q{alias $a $1},
      %q{         ~~ location})
  end

  #
  # Formal arguments
  #

  def test_arg
    assert_parses(
      s(:def, :f,
        s(:args, s(:arg, :foo)),
        nil),
      %q{def f(foo); end},
      %q{      ~~~ name (args.arg)
        |      ~~~ expression (args.arg)
        |     ^ begin (args)
        |         ^ end (args)
        |     ~~~~~ expression (args)})

    assert_parses(
      s(:def, :f,
        s(:args, s(:arg, :foo), s(:arg, :bar)),
        nil),
      %q{def f(foo, bar); end})
  end

  def test_optarg
    assert_parses(
      s(:def, :f,
        s(:args, s(:optarg, :foo, s(:int, 1))),
        nil),
      %q{def f foo = 1; end},
      %q{      ~~~ name (args.optarg)
        |          ^ operator (args.optarg)
        |      ~~~~~~~ expression (args.optarg)
        |      ~~~~~~~ expression (args)})

    assert_parses(
      s(:def, :f,
        s(:args,
          s(:optarg, :foo, s(:int, 1)),
          s(:optarg, :bar, s(:int, 2))),
        nil),
      %q{def f(foo=1, bar=2); end})
  end

  def test_restarg_named
    assert_parses(
      s(:def, :f,
        s(:args, s(:restarg, :foo)),
        nil),
      %q{def f(*foo); end},
      %q{       ~~~ name (args.restarg)
        |      ~~~~ expression (args.restarg)})
  end

  def test_restarg_unnamed
    assert_parses(
      s(:def, :f,
        s(:args, s(:restarg)),
        nil),
      %q{def f(*); end},
      %q{      ~ expression (args.restarg)})
  end

  def test_kwarg
    assert_parses(
      s(:def, :f,
        s(:args, s(:kwarg, :foo)),
        nil),
      %q{def f(foo:); end},
      %q{      ~~~ name (args.kwarg)
        |      ~~~~ expression (args.kwarg)},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def test_kwoptarg
    assert_parses(
      s(:def, :f,
        s(:args, s(:kwoptarg, :foo, s(:int, 1))),
        nil),
      %q{def f(foo: 1); end},
      %q{      ~~~ name (args.kwoptarg)
        |      ~~~~~~ expression (args.kwoptarg)},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_kwrestarg_named
    assert_parses(
      s(:def, :f,
        s(:args, s(:kwrestarg, :foo)),
        nil),
      %q{def f(**foo); end},
      %q{        ~~~ name (args.kwrestarg)
        |      ~~~~~ expression (args.kwrestarg)},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_kwrestarg_unnamed
    assert_parses(
      s(:def, :f,
        s(:args, s(:kwrestarg)),
        nil),
      %q{def f(**); end},
      %q{      ~~ expression (args.kwrestarg)},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_blockarg
    assert_parses(
      s(:def, :f,
        s(:args, s(:blockarg, :block)),
        nil),
      %q{def f(&block); end},
      %q{       ~~~~~ name (args.blockarg)
        |      ~~~~~~ expression (args.blockarg)})
  end

  def test_arg_scope
    # [ruby-core:61299] [Bug #9593]
    assert_parses(
      s(:def, :f,
        s(:args, s(:optarg, :var, s(:defined?, s(:lvar, :var)))),
        s(:lvar, :var)),
      %q{def f(var = defined?(var)) var end},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9 2.0))

    assert_parses(
      s(:def, :f,
        s(:args, s(:kwoptarg, :var, s(:defined?, s(:lvar, :var)))),
        s(:lvar, :var)),
      %q{def f(var: defined?(var)) var end},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def assert_parses_args(ast, code, versions=ALL_VERSIONS)
    assert_parses(
      s(:def, :f, ast, nil),
      %Q{def f #{code}; end},
      %q{},
      versions)
  end

  def test_arg_combinations
    # f_arg tCOMMA f_optarg tCOMMA f_rest_arg              opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:restarg, :r),
        s(:blockarg, :b)),
      %q{a, o=1, *r, &b})

    # f_arg tCOMMA f_optarg tCOMMA f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{a, o=1, *r, p, &b},
      ALL_VERSIONS - %w(1.8))

    # f_arg tCOMMA f_optarg                                opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:blockarg, :b)),
      %q{a, o=1, &b})

    # f_arg tCOMMA f_optarg tCOMMA                   f_arg opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{a, o=1, p, &b},
      ALL_VERSIONS - %w(1.8))

    # f_arg tCOMMA                 f_rest_arg              opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:arg, :a),
        s(:restarg, :r),
        s(:blockarg, :b)),
      %q{a, *r, &b})

    # f_arg tCOMMA                 f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:arg, :a),
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{a, *r, p, &b},
      ALL_VERSIONS - %w(1.8))

    # f_arg                                                opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:arg, :a),
        s(:blockarg, :b)),
      %q{a, &b})

    #              f_optarg tCOMMA f_rest_arg              opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:restarg, :r),
        s(:blockarg, :b)),
      %q{o=1, *r, &b})

    #              f_optarg tCOMMA f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{o=1, *r, p, &b},
      ALL_VERSIONS - %w(1.8))

    #              f_optarg                                opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:blockarg, :b)),
      %q{o=1, &b})

    #              f_optarg tCOMMA                   f_arg opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{o=1, p, &b},
      ALL_VERSIONS - %w(1.8))

    #                              f_rest_arg              opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:restarg, :r),
        s(:blockarg, :b)),
      %q{*r, &b})

    #                              f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{*r, p, &b},
      ALL_VERSIONS - %w(1.8))

    #                                                          f_block_arg
    assert_parses_args(
      s(:args,
        s(:blockarg, :b)),
      %q{&b})

    # (nothing)
    assert_parses_args(
      s(:args),
      %q{})
  end

  def test_kwarg_combinations
    # f_kwarg tCOMMA f_kwrest opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:kwoptarg, :foo, s(:int, 1)),
        s(:kwoptarg, :bar, s(:int, 2)),
        s(:kwrestarg, :baz),
        s(:blockarg, :b)),
      %q{(foo: 1, bar: 2, **baz, &b)},
      ALL_VERSIONS - %w(1.8 1.9))

    # f_kwarg opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:kwoptarg, :foo, s(:int, 1)),
        s(:blockarg, :b)),
      %q{(foo: 1, &b)},
      ALL_VERSIONS - %w(1.8 1.9))

    # f_kwrest opt_f_block_arg
    assert_parses_args(
      s(:args,
        s(:kwrestarg, :baz),
        s(:blockarg, :b)),
      %q{**baz, &b},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses_args(
      s(:args,
        s(:restarg),
        s(:kwrestarg)),
      %q{*, **},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_kwarg_no_paren
    assert_parses_args(
      s(:args,
        s(:kwarg, :foo)),
      %Q{foo:\n},
      ALL_VERSIONS - %w(1.8 1.9 2.0))

    assert_parses_args(
      s(:args,
        s(:kwoptarg, :foo, s(:int, -1))),
      %Q{foo: -1\n},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def assert_parses_margs(ast, code, versions=ALL_VERSIONS - %w(1.8))
    assert_parses_args(
      s(:args, ast),
      %Q{(#{code})},
      versions)
  end

  def test_marg_combinations
    # tLPAREN f_margs rparen
    assert_parses_margs(
      s(:mlhs,
        s(:mlhs, s(:arg, :a))),
      %q{((a))})

    # f_marg_list
    assert_parses_margs(
      s(:mlhs, s(:arg, :a), s(:arg, :a1)),
      %q{(a, a1)})

    # f_marg_list tCOMMA tSTAR f_norm_arg
    assert_parses_margs(
      s(:mlhs, s(:arg, :a), s(:restarg, :r)),
      %q{(a, *r)})

    # f_marg_list tCOMMA tSTAR f_norm_arg tCOMMA f_marg_list
    assert_parses_margs(
      s(:mlhs, s(:arg, :a), s(:restarg, :r), s(:arg, :p)),
      %q{(a, *r, p)})

    # f_marg_list tCOMMA tSTAR
    assert_parses_margs(
      s(:mlhs, s(:arg, :a), s(:restarg)),
      %q{(a, *)})

    # f_marg_list tCOMMA tSTAR            tCOMMA f_marg_list
    assert_parses_margs(
      s(:mlhs, s(:arg, :a), s(:restarg), s(:arg, :p)),
      %q{(a, *, p)})

    # tSTAR f_norm_arg
    assert_parses_margs(
      s(:mlhs, s(:restarg, :r)),
      %q{(*r)})

    # tSTAR f_norm_arg tCOMMA f_marg_list
    assert_parses_margs(
      s(:mlhs, s(:restarg, :r), s(:arg, :p)),
      %q{(*r, p)})

    # tSTAR
    assert_parses_margs(
      s(:mlhs, s(:restarg)),
      %q{(*)})

    # tSTAR tCOMMA f_marg_list
    assert_parses_margs(
      s(:mlhs, s(:restarg), s(:arg, :p)),
      %q{(*, p)})
  end

  def assert_parses_blockargs(ast, code, versions=ALL_VERSIONS)
    assert_parses(
      s(:block,
        s(:send, nil, :f),
        ast, nil),
      %Q{f{ #{code} }},
      %q{},
      versions)
  end

  def test_block_arg_combinations
    # none
    assert_parses_blockargs(
      s(:args),
      %q{})

    # tPIPE tPIPE
    # tPIPE opt_bv_decl tPIPE
    assert_parses_blockargs(
      s(:args),
      %q{| |})

    assert_parses_blockargs(
      s(:args, s(:shadowarg, :a)),
      %q{|;a|},
      ALL_VERSIONS - %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:shadowarg, :a)),
      %Q{|;\na\n|},
      ALL_VERSIONS - %w(1.8 1.9))

    # tOROP
    assert_parses_blockargs(
      s(:args),
      %q{||})

    # block_par
    # block_par tCOMMA
    # block_par tCOMMA tAMPER lhs
    # f_arg                                                      opt_f_block_arg
    # f_arg tCOMMA
    assert_parses_blockargs(
      s(:args, s(:arg, :a)),
      %q{|a|})

    assert_parses_blockargs(
      s(:args, s(:arg, :a), s(:arg, :c)),
      %q{|a, c|})

    assert_parses_blockargs(
      s(:args, s(:arg_expr, s(:ivasgn, :@a))),
      %q{|@a|},
      %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:arg, :a)),
      %q{|a,|})

    assert_parses_blockargs(
      s(:args, s(:arg, :a), s(:blockarg, :b)),
      %q{|a, &b|})

    assert_parses_blockargs(
      s(:args, s(:arg, :a), s(:blockarg_expr, s(:ivasgn, :@b))),
      %q{|a, &@b|},
      %w(1.8))

    # block_par tCOMMA tSTAR lhs tCOMMA tAMPER lhs
    # block_par tCOMMA tSTAR tCOMMA tAMPER lhs
    # block_par tCOMMA tSTAR lhs
    # block_par tCOMMA tSTAR
    # f_arg tCOMMA                       f_rest_arg              opt_f_block_arg
    assert_parses_blockargs(
      s(:args, s(:arg, :a), s(:restarg, :s), s(:blockarg, :b)),
      %q{|a, *s, &b|})

    assert_parses_blockargs(
      s(:args, s(:arg, :a),
        s(:restarg_expr, s(:ivasgn, :@s)),
        s(:blockarg_expr, s(:ivasgn, :@b))),
      %q{|a, *@s, &@b|},
      %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:arg, :a), s(:restarg), s(:blockarg, :b)),
      %q{|a, *, &b|})

    assert_parses_blockargs(
      s(:args, s(:arg, :a),
        s(:restarg),
        s(:blockarg_expr, s(:ivasgn, :@b))),
      %q{|a, *, &@b|},
      %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:arg, :a), s(:restarg, :s)),
      %q{|a, *s|})

    assert_parses_blockargs(
      s(:args, s(:arg, :a),
        s(:restarg_expr, s(:ivasgn, :@s))),
      %q{|a, *@s|},
      %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:arg, :a), s(:restarg)),
      %q{|a, *|})

    # tSTAR lhs tCOMMA tAMPER lhs
    # tSTAR lhs
    # tSTAR
    # tSTAR tCOMMA tAMPER lhs
    #                                    f_rest_arg              opt_f_block_arg
    assert_parses_blockargs(
      s(:args, s(:restarg, :s), s(:blockarg, :b)),
      %q{|*s, &b|})

    assert_parses_blockargs(
      s(:args,
        s(:restarg_expr, s(:ivasgn, :@s)),
        s(:blockarg_expr, s(:ivasgn, :@b))),
      %q{|*@s, &@b|},
      %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:restarg), s(:blockarg, :b)),
      %q{|*, &b|})

    assert_parses_blockargs(
      s(:args,
        s(:restarg),
        s(:blockarg_expr, s(:ivasgn, :@b))),
      %q{|*, &@b|},
      %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:restarg, :s)),
      %q{|*s|})

    assert_parses_blockargs(
      s(:args,
        s(:restarg_expr, s(:ivasgn, :@s))),
      %q{|*@s|},
      %w(1.8))

    assert_parses_blockargs(
      s(:args, s(:restarg)),
      %q{|*|})

    # tAMPER lhs
    #                                                                f_block_arg
    assert_parses_blockargs(
      s(:args, s(:blockarg, :b)),
      %q{|&b|})

    assert_parses_blockargs(
      s(:args,
        s(:blockarg_expr, s(:ivasgn, :@b))),
      %q{|&@b|},
      %w(1.8))

    # f_arg tCOMMA f_block_optarg tCOMMA f_rest_arg              opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:optarg, :o1, s(:int, 2)),
        s(:restarg, :r),
        s(:blockarg, :b)),
      %q{|a, o=1, o1=2, *r, &b|},
      ALL_VERSIONS - %w(1.8))

    # f_arg tCOMMA f_block_optarg tCOMMA f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{|a, o=1, *r, p, &b|},
      ALL_VERSIONS - %w(1.8))

    # f_arg tCOMMA f_block_optarg                                opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:blockarg, :b)),
      %q{|a, o=1, &b|},
      ALL_VERSIONS - %w(1.8))

    # f_arg tCOMMA f_block_optarg tCOMMA                   f_arg opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:arg, :a),
        s(:optarg, :o, s(:int, 1)),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{|a, o=1, p, &b|},
      ALL_VERSIONS - %w(1.8))

    # f_arg tCOMMA                       f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:arg, :a),
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{|a, *r, p, &b|},
      ALL_VERSIONS - %w(1.8))

    #              f_block_optarg tCOMMA f_rest_arg              opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:restarg, :r),
        s(:blockarg, :b)),
      %q{|o=1, *r, &b|},
      ALL_VERSIONS - %w(1.8))

    #              f_block_optarg tCOMMA f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{|o=1, *r, p, &b|},
      ALL_VERSIONS - %w(1.8))

    #              f_block_optarg                                opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:blockarg, :b)),
      %q{|o=1, &b|},
      ALL_VERSIONS - %w(1.8))

    #              f_block_optarg tCOMMA                   f_arg opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:optarg, :o, s(:int, 1)),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{|o=1, p, &b|},
      ALL_VERSIONS - %w(1.8))

    #                                    f_rest_arg tCOMMA f_arg opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:restarg, :r),
        s(:arg, :p),
        s(:blockarg, :b)),
      %q{|*r, p, &b|},
      ALL_VERSIONS - %w(1.8))
  end

  def test_block_kwarg_combinations
    # f_block_kwarg tCOMMA f_kwrest opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:kwoptarg, :foo, s(:int, 1)),
        s(:kwoptarg, :bar, s(:int, 2)),
        s(:kwrestarg, :baz),
        s(:blockarg, :b)),
      %q{|foo: 1, bar: 2, **baz, &b|},
      ALL_VERSIONS - %w(1.8 1.9))

    # f_block_kwarg opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:kwoptarg, :foo, s(:int, 1)),
        s(:blockarg, :b)),
      %q{|foo: 1, &b|},
      ALL_VERSIONS - %w(1.8 1.9))

    # f_kwrest opt_f_block_arg
    assert_parses_blockargs(
      s(:args,
        s(:kwrestarg, :baz),
        s(:blockarg, :b)),
      %q{|**baz, &b|},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_block_kwarg
    assert_parses_blockargs(
      s(:args,
        s(:kwarg, :foo)),
      %q{|foo:|},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def test_arg_invalid
    assert_diagnoses(
      [:error, :argument_const],
      %q{def foo(Abc); end},
      %q{        ~~~ location})

    assert_diagnoses(
      [:error, :argument_ivar],
      %q{def foo(@abc); end},
      %q{        ~~~~ location})

    assert_diagnoses(
      [:error, :argument_gvar],
      %q{def foo($abc); end},
      %q{        ~~~~ location})

    assert_diagnoses(
      [:error, :argument_cvar],
      %q{def foo(@@abc); end},
      %q{        ~~~~~ location})
  end

  def test_arg_duplicate
    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, aa); end},
      %q{            ^^ location
        |        ~~ highlights (0)})

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, aa=1); end},
      %q{            ^^ location
        |        ~~ highlights (0)})

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, *aa); end},
      %q{             ^^ location
        |        ~~ highlights (0)})

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, &aa); end},
      %q{             ^^ location
        |        ~~ highlights (0)})

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, (bb, aa)); end},
      %q{                 ^^ location
        |        ~~ highlights (0)},
      ALL_VERSIONS - %w(1.8))

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, *r, aa); end},
      %q{                ^^ location
        |        ~~ highlights (0)},
      ALL_VERSIONS - %w(1.8))


    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{lambda do |aa; aa| end},
      %q{               ^^ location
        |           ~~ highlights (0)},
      ALL_VERSIONS - %w(1.8))

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, aa: 1); end},
      %q{            ^^ location
        |        ~~ highlights (0)},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, **aa); end},
      %q{              ^^ location
        |        ~~ highlights (0)},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(aa, aa:); end},
      %q{            ^^ location
        |        ~~ highlights (0)},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def test_arg_duplicate_ignored
    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(_, _); end},
      %q{},
      %w(1.8))

    assert_parses(
      s(:def, :foo,
        s(:args, s(:arg, :_), s(:arg, :_)),
        nil),
      %q{def foo(_, _); end},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{def foo(_a, _a); end},
      %q{},
      %w(1.8 1.9))

    assert_parses(
      s(:def, :foo,
        s(:args, s(:arg, :_a), s(:arg, :_a)),
        nil),
      %q{def foo(_a, _a); end},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_arg_duplicate_proc
    assert_parses(
      s(:block, s(:send, nil, :proc),
        s(:args, s(:arg, :a), s(:arg, :a)),
        nil),
      %q{proc{|a,a|}},
      %q{},
      %w(1.8))

    assert_diagnoses(
      [:error, :duplicate_argument],
      %q{proc{|a,a|}},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_kwarg_invalid
    assert_diagnoses(
      [:error, :argument_const],
      %q{def foo(Abc: 1); end},
      %q{        ~~~~ location},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_diagnoses(
      [:error, :argument_const],
      %q{def foo(Abc:); end},
      %q{        ~~~~ location},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def test_arg_label
    assert_parses(
      s(:def, :foo, s(:args),
        s(:send, nil, :a, s(:sym, :b))),
      %q{def foo() a:b end},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:def, :foo, s(:args),
        s(:send, nil, :a, s(:sym, :b))),
      %Q{def foo\n a:b end},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:block,
        s(:send, nil, :f),
        s(:args),
        s(:send, nil, :a,
          s(:sym, :b))),
      %Q{f { || a:b }},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  #
  # Sends
  #

  # To self

  def test_send_self
    assert_parses(
      s(:send, nil, :fun),
      %q{fun},
      %q{~~~ selector
        |~~~ expression})

    assert_parses(
      s(:send, nil, :fun!),
      %q{fun!},
      %q{~~~~ selector
        |~~~~ expression})

    assert_parses(
      s(:send, nil, :fun, s(:int, 1)),
      %q{fun(1)},
      %q{~~~ selector
        |   ^ begin
        |     ^ end
        |~~~~~~ expression})
  end

  def test_send_self_block
    assert_parses(
      s(:block, s(:send, nil, :fun), s(:args), nil),
      %q{fun { }})

    assert_parses(
      s(:block, s(:send, nil, :fun), s(:args), nil),
      %q{fun() { }})

    assert_parses(
      s(:block, s(:send, nil, :fun, s(:int, 1)), s(:args), nil),
      %q{fun(1) { }})

    assert_parses(
      s(:block, s(:send, nil, :fun), s(:args), nil),
      %q{fun do end})
  end

  def test_send_block_blockarg
    assert_diagnoses(
      [:error, :block_and_blockarg],
      %q{fun(&bar) do end},
      %q{    ~~~~ location
        |          ~~ highlights (0)})
  end

  # To receiver

  def test_send_plain
    assert_parses(
      s(:send, s(:lvar, :foo), :fun),
      %q{foo.fun},
      %q{    ~~~ selector
        |   ^ dot
        |~~~~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :fun),
      %q{foo::fun},
      %q{     ~~~ selector
        |   ^^ dot
        |~~~~~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :Fun),
      %q{foo::Fun()},
      %q{     ~~~ selector
        |   ^^ dot
        |~~~~~~~~~~ expression})
  end

  def test_send_plain_cmd
    assert_parses(
      s(:send, s(:lvar, :foo), :fun, s(:lvar, :bar)),
      %q{foo.fun bar},
      %q{    ~~~ selector
        |   ^ dot
        |~~~~~~~~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :fun, s(:lvar, :bar)),
      %q{foo::fun bar},
      %q{     ~~~ selector
        |   ^^ dot
        |~~~~~~~~~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :Fun, s(:lvar, :bar)),
      %q{foo::Fun bar},
      %q{     ~~~ selector
        |   ^^ dot
        |~~~~~~~~~~~~ expression})
  end

  def test_send_plain_cmd_ambiguous_literal
    assert_diagnoses(
      [:warning, :ambiguous_literal],
      %q{m /foo/},
      %q{  ^ location})

    refute_diagnoses(
      %q{m %[1]})
  end

  def test_send_plain_cmd_ambiguous_prefix
    assert_diagnoses(
      [:warning, :ambiguous_prefix, { :prefix => '+' }],
      %q{m +foo},
      %q{  ^ location})

    assert_diagnoses(
      [:warning, :ambiguous_prefix, { :prefix => '-' }],
      %q{m -foo},
      %q{  ^ location})

    assert_diagnoses(
      [:warning, :ambiguous_prefix, { :prefix => '&' }],
      %q{m &foo},
      %q{  ^ location})

    assert_diagnoses(
      [:warning, :ambiguous_prefix, { :prefix => '*' }],
      %q{m *foo},
      %q{  ^ location})

    assert_diagnoses(
      [:warning, :ambiguous_prefix, { :prefix => '**' }],
      %q{m **foo},
      %q{  ^^ location},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_send_block_chain_cmd
    assert_parses(
      s(:send,
        s(:block,
          s(:send, nil, :meth, s(:int, 1)),
          s(:args), nil),
        :fun, s(:lvar, :bar)),
      %q{meth 1 do end.fun bar},
      %q{              ~~~ selector
        |             ^ dot
        |~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:send,
        s(:block,
          s(:send, nil, :meth, s(:int, 1)),
          s(:args), nil),
        :fun, s(:lvar, :bar)),
      %q{meth 1 do end.fun(bar)},
      %q{              ~~~ selector
        |             ^ dot
        |                 ^ begin
        |                     ^ end
        |~~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:send,
        s(:block,
          s(:send, nil, :meth, s(:int, 1)),
          s(:args), nil),
        :fun, s(:lvar, :bar)),
      %q{meth 1 do end::fun bar},
      %q{               ~~~ selector
        |             ^^ dot
        |~~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:send,
        s(:block,
          s(:send, nil, :meth, s(:int, 1)),
          s(:args), nil),
        :fun, s(:lvar, :bar)),
      %q{meth 1 do end::fun(bar)},
      %q{               ~~~ selector
        |                  ^ begin
        |                      ^ end
        |             ^^ dot
        |~~~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:block,
        s(:send,
          s(:block,
            s(:send, nil, :meth, s(:int, 1)),
            s(:args), nil),
          :fun, s(:lvar, :bar)),
        s(:args), nil),
      %q{meth 1 do end.fun bar do end},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses(
      s(:block,
        s(:send,
          s(:block,
            s(:send, nil, :meth, s(:int, 1)),
            s(:args), nil),
          :fun, s(:lvar, :bar)),
        s(:args), nil),
      %q{meth 1 do end.fun(bar) {}},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))

    assert_parses(
      s(:block,
        s(:send,
          s(:block,
            s(:send, nil, :meth, s(:int, 1)),
            s(:args), nil),
          :fun),
        s(:args), nil),
      %q{meth 1 do end.fun {}},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_send_paren_block_cmd
    assert_parses(
      s(:send, nil, :foo,
        s(:block,
          s(:send, nil, :meth, s(:int, 1)),
          s(:args), nil)),
      %q{foo(meth 1 do end)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :foo,
        s(:int, 1),
        s(:block,
          s(:send, nil, :meth, s(:int, 1)),
          s(:args), nil)),
      %q{foo(1, meth 1 do end)},
      %q{},
      %w(1.8))
  end

  def test_send_binary_op
    assert_parses(
      s(:send, s(:lvar, :foo), :+, s(:int, 1)),
      %q{foo + 1},
      %q{    ~ selector
        |~~~~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :-, s(:int, 1)),
      %q{foo - 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :*, s(:int, 1)),
      %q{foo * 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :/, s(:int, 1)),
      %q{foo / 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :%, s(:int, 1)),
      %q{foo % 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :**, s(:int, 1)),
      %q{foo ** 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :|, s(:int, 1)),
      %q{foo | 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :^, s(:int, 1)),
      %q{foo ^ 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :&, s(:int, 1)),
      %q{foo & 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :<=>, s(:int, 1)),
      %q{foo <=> 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :<, s(:int, 1)),
      %q{foo < 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :<=, s(:int, 1)),
      %q{foo <= 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :>, s(:int, 1)),
      %q{foo > 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :>=, s(:int, 1)),
      %q{foo >= 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :==, s(:int, 1)),
      %q{foo == 1})

    assert_parses(
      s(:not, s(:send, s(:lvar, :foo), :==, s(:int, 1))),
      %q{foo != 1},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :'!=', s(:int, 1)),
      %q{foo != 1},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :===, s(:int, 1)),
      %q{foo === 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :=~, s(:int, 1)),
      %q{foo =~ 1})

    assert_parses(
      s(:not, s(:send, s(:lvar, :foo), :=~, s(:int, 1))),
      %q{foo !~ 1},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :'!~', s(:int, 1)),
      %q{foo !~ 1},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :<<, s(:int, 1)),
      %q{foo << 1})

    assert_parses(
      s(:send, s(:lvar, :foo), :>>, s(:int, 1)),
      %q{foo >> 1})
  end

  def test_send_unary_op
    assert_parses(
      s(:send, s(:lvar, :foo), :-@),
      %q{-foo},
      %q{~ selector
        |~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :+@),
      %q{+foo})

    assert_parses(
      s(:send, s(:lvar, :foo), :~),
      %q{~foo})
  end

  def test_bang
    assert_parses(
      s(:not, s(:lvar, :foo)),
      %q{!foo},
      %{},
      %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :'!'),
      %q{!foo},
      %{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_bang_cmd
    assert_parses(
      s(:not, s(:send, nil, :m, s(:lvar, :foo))),
      %q{!m foo},
      %{},
      %w(1.8))

    assert_parses(
      s(:send, s(:send, nil, :m, s(:lvar, :foo)), :'!'),
      %q{!m foo},
      %{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_not
    assert_parses(
      s(:not, s(:lvar, :foo)),
      %q{not foo},
      %{},
      %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :'!'),
      %q{not foo},
      %{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :'!'),
      %q{not(foo)},
      %q{~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:send, s(:begin), :'!'),
      %q{not()},
      %q{~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_not_cmd
    assert_parses(
      s(:not, s(:send, nil, :m, s(:lvar, :foo))),
      %q{not m foo},
      %{},
      %w(1.8))

    assert_parses(
      s(:send, s(:send, nil, :m, s(:lvar, :foo)), :'!'),
      %q{not m foo},
      %{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_pow_precedence
    assert_parses(
      s(:send, s(:send, s(:int, 2), :**, s(:int, 10)), :-@),
      %q{-2 ** 10})

    assert_parses(
      s(:send, s(:send, s(:float, 2.0), :**, s(:int, 10)), :-@),
      %q{-2.0 ** 10})
  end

  def test_send_attr_asgn
    assert_parses(
      s(:send, s(:lvar, :foo), :a=, s(:int, 1)),
      %q{foo.a = 1},
      %q{    ~ selector
        |   ^ dot
        |      ^ operator
        |~~~~~~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :a=, s(:int, 1)),
      %q{foo::a = 1},
      %q{     ~ selector
        |   ^^ dot
        |       ^ operator
        |~~~~~~~~~~ expression})

    assert_parses(
      s(:send, s(:lvar, :foo), :A=, s(:int, 1)),
      %q{foo.A = 1},
      %q{    ~ selector
        |   ^ dot
        |      ^ operator
        |~~~~~~~~~ expression})

    assert_parses(
      s(:casgn, s(:lvar, :foo), :A, s(:int, 1)),
      %q{foo::A = 1},
      %q{     ~ name
        |   ^^ double_colon
        |       ^ operator
        |~~~~~~~~~~ expression})
  end

  def test_send_index
    assert_parses(
      s(:send, s(:lvar, :foo), :[],
        s(:int, 1), s(:int, 2)),
      %q{foo[1, 2]},
      %q{   ~~~~~~ selector
        |~~~~~~~~~ expression})
  end

  def test_send_index_cmd
    assert_parses(
      s(:send, s(:lvar, :foo), :[],
        s(:send, nil, :m, s(:lvar, :bar))),
      %q{foo[m bar]})
  end

  def test_send_index_asgn
    assert_parses(
      s(:send, s(:lvar, :foo), :[]=,
        s(:int, 1), s(:int, 2), s(:int, 3)),
      %q{foo[1, 2] = 3},
      %q{   ~~~~~~ selector
        |          ^ operator
        |~~~~~~~~~~~~~ expression})
  end

  def test_send_lambda
    assert_parses(
      s(:block, s(:send, nil, :lambda),
        s(:args), nil),
      %q{->{ }},
      %q{~~ selector (send)
        |  ^ begin
        |    ^ end
        |~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:block, s(:send, nil, :lambda),
        s(:args, s(:restarg)), nil),
      %q{-> * { }},
      %q{~~ selector (send)
        |     ^ begin
        |       ^ end
        |   ^ expression (args.restarg)
        |~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:block, s(:send, nil, :lambda),
        s(:args), nil),
      %q{-> do end},
      %q{~~ selector (send)
        |   ^^ begin
        |      ^^^ end
        |~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_send_lambda_args
    assert_parses(
      s(:block, s(:send, nil, :lambda),
        s(:args,
          s(:arg, :a)),
        nil),
      %q{->(a) { }},
      %q{~~ selector (send)
        |  ^ begin (args)
        |    ^ end (args)
        |      ^ begin
        |        ^ end
        |~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:block, s(:send, nil, :lambda),
        s(:args,
          s(:arg, :a)),
        nil),
      %q{-> (a) { }},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_send_lambda_args_shadow
    assert_parses(
      s(:block, s(:send, nil, :lambda),
        s(:args,
          s(:arg, :a),
          s(:shadowarg, :foo),
          s(:shadowarg, :bar)),
        nil),
      %q{->(a; foo, bar) { }},
      %q{      ~~~ expression (args.shadowarg)},
      ALL_VERSIONS - %w(1.8))
  end

  def test_send_call
    assert_parses(
      s(:send, s(:lvar, :foo), :call,
        s(:int, 1)),
      %q{foo.(1)},
      %q{    ^ begin
        |      ^ end
        |   ^ dot
        |~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:send, s(:lvar, :foo), :call,
        s(:int, 1)),
      %q{foo::(1)},
      %q{     ^ begin
        |       ^ end
        |   ^^ dot
        |~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_lvar_injecting_match
    assert_parses(
      s(:begin,
        s(:match_with_lvasgn,
          s(:regexp,
            s(:str, '(?<match>bar)'),
            s(:regopt)),
          s(:str, 'bar')),
        s(:lvar, :match)),
      %q{/(?<match>bar)/ =~ 'bar'; match},
      %q{                ~~ selector (match_with_lvasgn)
        |~~~~~~~~~~~~~~~~~~~~~~~~ expression (match_with_lvasgn)},
      ALL_VERSIONS - %w(1.8))
  end

  def test_non_lvar_injecting_match
    assert_parses(
      s(:send,
        s(:regexp,
          s(:begin, s(:int, 1)),
          s(:str, '(?<match>bar)'),
          s(:regopt)),
        :=~,
        s(:str, 'bar')),
      %q{/#{1}(?<match>bar)/ =~ 'bar'})
  end

  # To superclass

  def test_super
    assert_parses(
      s(:super, s(:lvar, :foo)),
      %q{super(foo)},
      %q{~~~~~ keyword
        |     ^ begin
        |         ^ end
        |~~~~~~~~~~ expression})

    assert_parses(
      s(:super, s(:lvar, :foo)),
      %q{super foo},
      %q{~~~~~ keyword
        |~~~~~~~~~ expression})

    assert_parses(
      s(:super),
      %q{super()},
      %q{~~~~~ keyword
        |     ^ begin
        |      ^ end
        |~~~~~~~ expression})
  end

  def test_zsuper
    assert_parses(
      s(:zsuper),
      %q{super},
      %q{~~~~~ keyword
        |~~~~~ expression})
  end

  def test_super_block
    assert_parses(
      s(:block,
        s(:super, s(:lvar, :foo), s(:lvar, :bar)),
        s(:args), nil),
      %q{super foo, bar do end})

    assert_parses(
      s(:block,
        s(:zsuper),
        s(:args), nil),
      %q{super do end})
  end

  # To block argument

  def test_yield
    assert_parses(
      s(:yield, s(:lvar, :foo)),
      %q{yield(foo)},
      %q{~~~~~ keyword
        |     ^ begin
        |         ^ end
        |~~~~~~~~~~ expression})

    assert_parses(
      s(:yield, s(:lvar, :foo)),
      %q{yield foo},
      %q{~~~~~ keyword
        |~~~~~~~~~ expression})

    assert_parses(
      s(:yield),
      %q{yield()},
      %q{~~~~~ keyword
        |     ^ begin
        |      ^ end
        |~~~~~~~ expression})

    assert_parses(
      s(:yield),
      %q{yield},
      %q{~~~~~ keyword
        |~~~~~ expression})
  end

  def test_yield_block
    assert_diagnoses(
      [:error, :block_given_to_yield],
      %q{yield foo do end},
      %q{~~~~~ location
        |          ~~ highlights (0)})

    assert_diagnoses(
      [:error, :block_given_to_yield],
      %q{yield(&foo)},
      %q{~~~~~ location
        |      ~~~~ highlights (0)})
  end

  # Call arguments

  def test_args_cmd
    assert_parses(
      s(:send, nil, :fun,
        s(:send, nil, :f, s(:lvar, :bar))),
      %q{fun(f bar)})
  end

  def test_args_args_star
    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:splat, s(:lvar, :bar))),
      %q{fun(foo, *bar)})

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun(foo, *bar, &baz)})
  end

  def test_args_star
    assert_parses(
      s(:send, nil, :fun,
        s(:splat, s(:lvar, :bar))),
      %q{fun(*bar)})

    assert_parses(
      s(:send, nil, :fun,
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun(*bar, &baz)})
  end

  def test_args_block_pass
    assert_parses(
      s(:send, nil, :fun,
        s(:block_pass, s(:lvar, :bar))),
      %q{fun(&bar)})
  end

  def test_args_args_comma
    assert_parses(
      s(:send, s(:lvar, :foo), :[],
        s(:lvar, :bar)),
      %q{foo[bar,]},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_args_assocs
    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1)))),
      %q{fun(:foo => 1)})

    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun(:foo => 1, &baz)})
  end

  def test_args_assocs_star
    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar))),
      %q{fun(:foo => 1, *bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun(:foo => 1, *bar, &baz)},
      %q{},
      %w(1.8))
  end

  def test_args_assocs_comma
    assert_parses(
      s(:send, s(:lvar, :foo), :[],
        s(:hash, s(:pair, s(:sym, :baz), s(:int, 1)))),
      %q{foo[:baz => 1,]},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_args_args_assocs
    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1)))),
      %q{fun(foo, :foo => 1)})

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun(foo, :foo => 1, &baz)})
  end

  def test_args_args_assocs_comma
    assert_parses(
      s(:send, s(:lvar, :foo), :[],
        s(:lvar, :bar),
        s(:hash, s(:pair, s(:sym, :baz), s(:int, 1)))),
      %q{foo[bar, :baz => 1,]},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_args_args_assocs_star
    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar))),
      %q{fun(foo, :foo => 1, *bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun(foo, :foo => 1, *bar, &baz)},
      %q{},
      %w(1.8))
  end

  # Call arguments with whitespace

  def test_space_args_cmd
    assert_parses(
      s(:send, nil, :fun,
        s(:begin, s(:send, nil, :f, s(:lvar, :bar)))),
      %q{fun (f bar)})
  end

  def test_space_args_arg
    assert_parses(
      s(:send, nil, :fun, s(:begin, s(:int, 1))),
      %q{fun (1)})
  end

  def test_space_args_arg_block
    assert_parses(
      s(:block,
        s(:send, nil, :fun, s(:begin, s(:int, 1))),
        s(:args), nil),
      %q{fun (1) {}})

    assert_parses(
      s(:block,
        s(:send, s(:lvar, :foo), :fun, s(:int, 1)),
        s(:args), nil),
      %q{foo.fun (1) {}},
      %q{},
      %w(1.8))

    assert_parses(
      s(:block,
        s(:send, s(:lvar, :foo), :fun, s(:begin, s(:int, 1))),
        s(:args), nil),
      %q{foo.fun (1) {}},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:block,
        s(:send, s(:lvar, :foo), :fun, s(:int, 1)),
        s(:args), nil),
      %q{foo::fun (1) {}},
      %q{},
      %w(1.8))

    assert_parses(
      s(:block,
        s(:send, s(:lvar, :foo), :fun, s(:begin, s(:int, 1))),
        s(:args), nil),
      %q{foo::fun (1) {}},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_space_args_arg_call
    assert_parses(
      s(:send, nil, :fun,
        s(:send, s(:begin, s(:int, 1)), :to_i)),
      %q{fun (1).to_i})
  end

  def test_space_args_block_pass
    assert_parses(
      s(:send, nil, :fun,
        s(:block_pass, s(:lvar, :foo))),
      %q{fun (&foo)},
      %q{},
      %w(1.8))
  end

  def test_space_args_arg_block_pass
    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:block_pass, s(:lvar, :bar))),
      %q{fun (foo, &bar)},
      %q{},
      %w(1.8))
  end

  def test_space_args_args_star
    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:splat, s(:lvar, :bar))),
      %q{fun (foo, *bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (foo, *bar, &baz)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:int, 1),
        s(:splat, s(:lvar, :bar))),
      %q{fun (foo, 1, *bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:int, 1),
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (foo, 1, *bar, &baz)},
      %q{},
      %w(1.8))
  end

  def test_space_args_star
    assert_parses(
      s(:send, nil, :fun,
        s(:splat, s(:lvar, :bar))),
      %q{fun (*bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (*bar, &baz)},
      %q{},
      %w(1.8))
  end

  def test_space_args_assocs
    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1)))),
      %q{fun (:foo => 1)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (:foo => 1, &baz)},
      %q{},
      %w(1.8))
  end

  def test_space_args_assocs_star
    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar))),
      %q{fun (:foo => 1, *bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (:foo => 1, *bar, &baz)},
      %q{},
      %w(1.8))
  end

  def test_space_args_args_assocs
    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1)))),
      %q{fun (foo, :foo => 1)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (foo, :foo => 1, &baz)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:int, 1),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1)))),
      %q{fun (foo, 1, :foo => 1)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:int, 1),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (foo, 1, :foo => 1, &baz)},
      %q{},
      %w(1.8))
  end

  def test_space_args_args_assocs_star
    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar))),
      %q{fun (foo, :foo => 1, *bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (foo, :foo => 1, *bar, &baz)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:int, 1),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar))),
      %q{fun (foo, 1, :foo => 1, *bar)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :fun,
        s(:lvar, :foo), s(:int, 1),
        s(:hash, s(:pair, s(:sym, :foo), s(:int, 1))),
        s(:splat, s(:lvar, :bar)),
        s(:block_pass, s(:lvar, :baz))),
      %q{fun (foo, 1, :foo => 1, *bar, &baz)},
      %q{},
      %w(1.8))
  end

  def test_space_args_arg_arg
    assert_parses(
      s(:send, nil, :fun, s(:int, 1), s(:int, 2)),
      %q{fun (1, 2)},
      %q{},
      %w(1.8))
  end

  def test_space_args_none
    assert_parses(
      s(:send, nil, :fun),
      %q{fun ()},
      %q{},
      %w(1.8))
  end

  def test_space_args_block
    assert_parses(
      s(:block,
        s(:send, nil, :fun),
        s(:args), nil),
      %q{fun () {}},
      %q{    ^ begin (send)
        |     ^ end (send)},
      %w(1.8))

    assert_parses(
      s(:block,
        s(:send, s(:lvar, :foo), :fun),
        s(:args), nil),
      %q{foo.fun () {}},
      %q{        ^ begin (send)
        |         ^ end (send)},
      %w(1.8))

    assert_parses(
      s(:block,
        s(:send, s(:lvar, :foo), :fun),
        s(:args), nil),
      %q{foo::fun () {}},
      %q{         ^ begin (send)
        |          ^ end (send)},
      %w(1.8))

    assert_parses(
      s(:block,
        s(:send, nil, :fun,
          s(:begin)),
        s(:args), nil),
      %q{fun () {}},
      %q{    ~~ expression (send.begin)},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  #
  # Control flow
  #

  # Operators

  def test_and
    assert_parses(
      s(:and, s(:lvar, :foo), s(:lvar, :bar)),
      %q{foo and bar},
      %q{    ~~~ operator
        |~~~~~~~~~~~ expression})

    assert_parses(
      s(:and, s(:lvar, :foo), s(:lvar, :bar)),
      %q{foo && bar},
      %q{    ~~ operator
        |~~~~~~~~~~ expression})
  end

  def test_or
    assert_parses(
      s(:or, s(:lvar, :foo), s(:lvar, :bar)),
      %q{foo or bar},
      %q{    ~~ operator
        |~~~~~~~~~~ expression})

    assert_parses(
      s(:or, s(:lvar, :foo), s(:lvar, :bar)),
      %q{foo || bar},
      %q{    ~~ operator
        |~~~~~~~~~~ expression})
  end

  def test_and_or_masgn
    assert_parses(
      s(:and,
        s(:lvar, :foo),
        s(:begin,
          s(:masgn,
            s(:mlhs, s(:lvasgn, :a), s(:lvasgn, :b)),
            s(:lvar, :bar)))),
      %q{foo && (a, b = bar)})

    assert_parses(
      s(:or,
        s(:lvar, :foo),
        s(:begin,
          s(:masgn,
            s(:mlhs, s(:lvasgn, :a), s(:lvasgn, :b)),
            s(:lvar, :bar)))),
      %q{foo || (a, b = bar)})
  end

  # Branching

  def test_if
    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :bar), nil),
      %q{if foo then bar; end},
      %q{~~ keyword
        |       ~~~~ begin
        |                 ~~~ end
        |~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :bar), nil),
      %q{if foo; bar; end},
      %q{~~ keyword
        |             ~~~ end
        |~~~~~~~~~~~~~~~~ expression})
  end

  def test_if_nl_then
    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :bar), nil),
      %Q{if foo\nthen bar end},
       %q{       ~~~~ begin})
  end

  def test_if_mod
    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :bar), nil),
      %q{bar if foo},
      %q{    ~~ keyword
        |~~~~~~~~~~ expression})
  end

  def test_unless
    assert_parses(
      s(:if, s(:lvar, :foo), nil, s(:lvar, :bar)),
      %q{unless foo then bar; end},
      %q{~~~~~~ keyword
        |           ~~~~ begin
        |                     ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:if, s(:lvar, :foo), nil, s(:lvar, :bar)),
      %q{unless foo; bar; end},
      %q{~~~~~~ keyword
        |                 ~~~ end
        |~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_unless_mod
    assert_parses(
      s(:if, s(:lvar, :foo), nil, s(:lvar, :bar)),
      %q{bar unless foo},
      %q{    ~~~~~~ keyword
        |~~~~~~~~~~~~~~ expression})
  end

  def test_if_else
    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :bar), s(:lvar, :baz)),
      %q{if foo then bar; else baz; end},
      %q{~~ keyword
        |       ~~~~ begin
        |                 ~~~~ else
        |                           ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :bar), s(:lvar, :baz)),
      %q{if foo; bar; else baz; end},
      %q{~~ keyword
        |             ~~~~ else
        |                       ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_unless_else
    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :baz), s(:lvar, :bar)),
      %q{unless foo then bar; else baz; end},
      %q{~~~~~~ keyword
        |           ~~~~ begin
        |                     ~~~~ else
        |                               ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :baz), s(:lvar, :bar)),
      %q{unless foo; bar; else baz; end},
      %q{~~~~~~ keyword
        |                 ~~~~ else
        |                           ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_if_elsif
    assert_parses(
      s(:if, s(:lvar, :foo), s(:lvar, :bar),
        s(:if, s(:lvar, :baz), s(:int, 1), s(:int, 2))),
      %q{if foo; bar; elsif baz; 1; else 2; end},
      %q{~~ keyword
        |             ~~~~~ else
        |             ~~~~~ keyword (if)
        |                           ~~~~ else (if)
        |                                   ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_ternary
    assert_parses(
      s(:if, s(:lvar, :foo), s(:int, 1), s(:int, 2)),
      %q{foo ? 1 : 2},
      %q{    ^ question
        |        ^ colon
        |~~~~~~~~~~~ expression})
  end

  def test_ternary_ambiguous_symbol
    assert_parses(
      s(:begin,
        s(:lvasgn, :t, s(:int, 1)),
        s(:if, s(:begin, s(:lvar, :foo)),
          s(:lvar, :t),
          s(:const, nil, :T))),
      %q{t=1;(foo)?t:T},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_if_masgn
    assert_diagnoses(
      [:error, :masgn_as_condition],
      %q{if (a, b = foo); end},
      %q{    ~~~~~~~~~~ location})
  end

  def test_if_mod_masgn
    assert_diagnoses(
      [:error, :masgn_as_condition],
      %q{1 if (a, b = foo)},
      %q{      ~~~~~~~~~~ location})
  end

  def test_tern_masgn
    assert_diagnoses(
      [:error, :masgn_as_condition],
      %q{(a, b = foo) ? 1 : 2},
      %q{ ~~~~~~~~~~ location})
  end

  def test_cond_begin
    assert_parses(
      s(:if,
        s(:begin, s(:lvar, :bar)),
        s(:lvar, :foo),
        nil),
      %q{if (bar); foo; end})
  end

  def test_cond_begin_masgn
    assert_parses(
      s(:if,
        s(:begin,
          s(:lvar, :bar),
          s(:masgn,
            s(:mlhs, s(:lvasgn, :a), s(:lvasgn, :b)),
            s(:lvar, :foo))),
        nil, nil),
      %q{if (bar; a, b = foo); end})
  end

  def test_cond_begin_and_or_masgn
    assert_diagnoses(
      [:error, :masgn_as_condition],
      %q{if foo && (a, b = bar); end},
      %q{           ~~~~~~~~~~ location},
      ALL_VERSIONS - %w(1.8))

    assert_diagnoses(
      [:error, :masgn_as_condition],
      %q{if foo || (a, b = bar); end},
      %q{           ~~~~~~~~~~ location},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:if,
        s(:and,
          s(:begin,
            s(:masgn,
              s(:mlhs,
                s(:lvasgn, :a), s(:lvasgn, :b)),
              s(:lvar, :foo))),
          s(:lvar, :bar)),
        nil, nil),
      %q{if (a, b = foo) && bar; end},
      %q{},
      %w(1.8))
  end

  def test_cond_iflipflop
    assert_parses(
      s(:if, s(:iflipflop, s(:lvar, :foo), s(:lvar, :bar)),
        nil, nil),
      %q{if foo..bar; end},
      %q{   ~~~~~~~~ expression (iflipflop)
        |      ~~ operator (iflipflop)})
  end

  def test_cond_eflipflop
    assert_parses(
      s(:if, s(:eflipflop, s(:lvar, :foo), s(:lvar, :bar)),
        nil, nil),
      %q{if foo...bar; end},
      %q{   ~~~~~~~~~ expression (eflipflop)
        |      ~~~ operator (eflipflop)})
  end

  def test_cond_match_current_line
    assert_parses(
      s(:if,
        s(:match_current_line,
          s(:regexp,
            s(:str, 'wat'),
            s(:regopt))),
        nil, nil),
      %q{if /wat/; end},
      %q{   ~~~~~ expression (match_current_line)})
  end

  # Case matching

  def test_case_expr
    assert_parses(
      s(:case, s(:lvar, :foo),
        s(:when, s(:str, 'bar'),
          s(:lvar, :bar)),
        nil),
      %q{case foo; when 'bar'; bar; end},
      %q{~~~~ keyword
        |          ~~~~ keyword (when)
        |                           ~~~ end
        |          ~~~~~~~~~~~~~~~ expression (when)
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_case_expr_else
    assert_parses(
      s(:case, s(:lvar, :foo),
        s(:when, s(:str, 'bar'),
          s(:lvar, :bar)),
        s(:lvar, :baz)),
      %q{case foo; when 'bar'; bar; else baz; end},
      %q{~~~~ keyword
        |          ~~~~ keyword (when)
        |                           ~~~~ else
        |                                     ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_case_cond
    assert_parses(
      s(:case, nil,
        s(:when, s(:lvar, :foo),
          s(:str, 'foo')),
        nil),
      %q{case; when foo; 'foo'; end},
      %q{~~~~ keyword
        |      ~~~~ keyword (when)
        |                       ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_case_cond_else
    assert_parses(
      s(:case, nil,
        s(:when, s(:lvar, :foo),
          s(:str, 'foo')),
        s(:str, 'bar')),
      %q{case; when foo; 'foo'; else 'bar'; end},
      %q{~~~~ keyword
        |      ~~~~ keyword (when)
        |                       ~~~~ else
        |                                   ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_case_cond_just_else
    assert_parses(
      s(:case, nil,
        s(:str, 'bar')),
      %q{case; else 'bar'; end},
      %q{~~~~ keyword
        |      ~~~~ else
        |                  ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~ expression},
      %w(1.8))
  end

  def test_when_then
    assert_parses(
      s(:case, s(:lvar, :foo),
        s(:when, s(:str, 'bar'),
          s(:lvar, :bar)),
        nil),
      %q{case foo; when 'bar' then bar; end},
      %q{          ~~~~ keyword (when)
        |                     ~~~~ begin (when)
        |          ~~~~~~~~~~~~~~~~~~~ expression (when)})
  end

  def test_when_multi
    assert_parses(
      s(:case, s(:lvar, :foo),
        s(:when, s(:str, 'bar'), s(:str, 'baz'),
          s(:lvar, :bar)),
        nil),
      %q{case foo; when 'bar', 'baz'; bar; end})
  end

  def test_when_splat
    assert_parses(
      s(:case, s(:lvar, :foo),
        s(:when,
          s(:int, 1),
          s(:splat, s(:lvar, :baz)),
          s(:lvar, :bar)),
        s(:when,
          s(:splat, s(:lvar, :foo)),
          nil),
        nil),
      %q{case foo; when 1, *baz; bar; when *foo; end},
      %q{                  ^ operator (when/1.splat)
        |                  ~~~~ expression (when/1.splat)
        |                                  ^ operator (when/2.splat)
        |                                  ~~~~ expression (when/2.splat)})
  end

  # Looping

  def test_while
    assert_parses(
      s(:while, s(:lvar, :foo), s(:send, nil, :meth)),
      %q{while foo do meth end},
      %q{~~~~~ keyword
        |          ~~ begin
        |                  ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:while, s(:lvar, :foo), s(:send, nil, :meth)),
      %q{while foo; meth end},
      %q{~~~~~ keyword
        |                ~~~ end
        |~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_while_mod
    assert_parses(
      s(:while, s(:lvar, :foo), s(:send, nil, :meth)),
      %q{meth while foo},
      %q{     ~~~~~ keyword})
  end

  def test_until
    assert_parses(
      s(:until, s(:lvar, :foo), s(:send, nil, :meth)),
      %q{until foo do meth end},
      %q{~~~~~ keyword
        |          ~~ begin
        |                  ~~~ end})

    assert_parses(
      s(:until, s(:lvar, :foo), s(:send, nil, :meth)),
      %q{until foo; meth end},
      %q{~~~~~ keyword
        |                ~~~ end})
  end

  def test_until_mod
    assert_parses(
      s(:until, s(:lvar, :foo), s(:send, nil, :meth)),
      %q{meth until foo},
      %q{     ~~~~~ keyword})
  end

  def test_while_post
    assert_parses(
      s(:while_post, s(:lvar, :foo),
        s(:kwbegin, s(:send, nil, :meth))),
      %q{begin meth end while foo},
      %q{               ~~~~~ keyword})
  end

  def test_until_post
    assert_parses(
      s(:until_post, s(:lvar, :foo),
        s(:kwbegin, s(:send, nil, :meth))),
      %q{begin meth end until foo},
      %q{               ~~~~~ keyword})
  end

  def test_while_masgn
    assert_diagnoses(
      [:error, :masgn_as_condition],
      %q{while (a, b = foo); end},
      %q{       ~~~~~~~~~~ location})
  end

  def test_while_mod_masgn
    assert_diagnoses(
      [:error, :masgn_as_condition],
      %q{foo while (a, b = foo)},
      %q{           ~~~~~~~~~~ location})
  end

  def test_for
    assert_parses(
      s(:for,
        s(:lvasgn, :a),
        s(:lvar, :foo),
        s(:send, nil, :p, s(:lvar, :a))),
      %q{for a in foo do p a; end},
      %q{~~~ keyword
        |      ~~ in
        |             ~~ begin
        |                     ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~ expression})

    assert_parses(
      s(:for,
        s(:lvasgn, :a),
        s(:lvar, :foo),
        s(:send, nil, :p, s(:lvar, :a))),
      %q{for a in foo; p a; end})
  end

  def test_for_mlhs
    assert_parses(
      s(:for,
        s(:mlhs,
          s(:lvasgn, :a),
          s(:lvasgn, :b)),
        s(:lvar, :foo),
        s(:send, nil, :p, s(:lvar, :a), s(:lvar, :b))),
      %q{for a, b in foo; p a, b; end},
      %q{    ~~~~ expression (mlhs)})
  end

  # Control flow commands

  def test_break
    assert_parses(
      s(:break, s(:begin, s(:lvar, :foo))),
      %q{break(foo)},
      %q{~~~~~ keyword
        |~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:break, s(:begin, s(:lvar, :foo))),
      %q{break(foo)},
      %q{~~~~~ keyword
        |~~~~~~~~~~ expression},
      %w(1.8))

    assert_parses(
      s(:break, s(:lvar, :foo)),
      %q{break foo},
      %q{~~~~~ keyword
        |~~~~~~~~~ expression})

    assert_parses(
        s(:break, s(:begin)),
      %q{break()},
      %q{~~~~~ keyword
        |~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:break),
      %q{break},
      %q{~~~~~ keyword
        |~~~~~ expression})
  end

  def test_break_block
    assert_parses(
      s(:break,
        s(:block,
          s(:send, nil, :fun, s(:lvar, :foo)),
          s(:args), nil)),
      %q{break fun foo do end},
      %q{      ~~~~~~~~~~~~~~ expression (block)
        |~~~~~~~~~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_return
    assert_parses(
      s(:return, s(:begin, s(:lvar, :foo))),
      %q{return(foo)},
      %q{~~~~~~ keyword
        |~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:return, s(:begin, s(:lvar, :foo))),
      %q{return(foo)},
      %q{~~~~~~ keyword
        |~~~~~~~~~~~ expression},
      %w(1.8))

    assert_parses(
      s(:return, s(:lvar, :foo)),
      %q{return foo},
      %q{~~~~~~ keyword
        |~~~~~~~~~~ expression})

    assert_parses(
      s(:return, s(:begin)),
      %q{return()},
      %q{~~~~~~ keyword
        |~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:return),
      %q{return},
      %q{~~~~~~ keyword
        |~~~~~~ expression})
  end

  def test_return_block
    assert_parses(
      s(:return,
        s(:block,
          s(:send, nil, :fun, s(:lvar, :foo)),
          s(:args), nil)),
      %q{return fun foo do end},
      %q{       ~~~~~~~~~~~~~~ expression (block)
        |~~~~~~~~~~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_next
    assert_parses(
      s(:next, s(:begin, s(:lvar, :foo))),
      %q{next(foo)},
      %q{~~~~ keyword
        |~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:next, s(:begin, s(:lvar, :foo))),
      %q{next(foo)},
      %q{~~~~ keyword
        |~~~~~~~~~ expression},
      %w(1.8))

    assert_parses(
      s(:next, s(:lvar, :foo)),
      %q{next foo},
      %q{~~~~ keyword
        |~~~~~~~~ expression})

    assert_parses(
        s(:next, s(:begin)),
      %q{next()},
      %q{~~~~ keyword
        |~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:next),
      %q{next},
      %q{~~~~ keyword
        |~~~~ expression})
  end

  def test_next_block
    assert_parses(
      s(:next,
        s(:block,
          s(:send, nil, :fun, s(:lvar, :foo)),
          s(:args), nil)),
      %q{next fun foo do end},
      %q{     ~~~~~~~~~~~~~~ expression (block)
        |~~~~~~~~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_redo
    assert_parses(
      s(:redo),
      %q{redo},
      %q{~~~~ keyword
        |~~~~ expression})
  end

  # Exception handling

  def test_rescue
    assert_parses(
      s(:kwbegin,
        s(:rescue, s(:send, nil, :meth),
          s(:resbody, nil, nil, s(:lvar, :foo)),
          nil)),
      %q{begin; meth; rescue; foo; end},
      %q{~~~~~ begin
        |             ~~~~~~ keyword (rescue.resbody)
        |             ~~~~~~~~~~~ expression (rescue.resbody)
        |       ~~~~~~~~~~~~~~~~~ expression (rescue)
        |                          ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_rescue_else
    assert_parses(
      s(:kwbegin,
        s(:rescue, s(:send, nil, :meth),
          s(:resbody, nil, nil, s(:lvar, :foo)),
          s(:lvar, :bar))),
      %q{begin; meth; rescue; foo; else; bar; end},
      %q{                          ~~~~ else (rescue)
        |       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression (rescue)})
  end

  def test_rescue_else_useless
    assert_diagnoses(
      [:warning, :useless_else],
      %q{begin; 1; else; 2; end},
      %q{          ~~~~ location})
  end

  def test_ensure
    assert_parses(
      s(:kwbegin,
        s(:ensure, s(:send, nil, :meth),
          s(:lvar, :bar))),
      %q{begin; meth; ensure; bar; end},
      %q{~~~~~ begin
        |             ~~~~~~ keyword (ensure)
        |       ~~~~~~~~~~~~~~~~~ expression (ensure)
        |                          ~~~ end
        |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_ensure_empty
    assert_parses(
      s(:kwbegin,
        s(:ensure, nil, nil)),
      %q{begin ensure end},
      %q{~~~~~ begin
        |      ~~~~~~ keyword (ensure)
        |      ~~~~~~ expression (ensure)
        |             ~~~ end
        |~~~~~~~~~~~~~~~~ expression})
  end

  def test_rescue_ensure
    assert_parses(
      s(:kwbegin,
        s(:ensure,
          s(:rescue,
            s(:send, nil, :meth),
            s(:resbody, nil, nil, s(:lvar, :baz)),
            nil),
          s(:lvar, :bar))),
      %q{begin; meth; rescue; baz; ensure; bar; end},
      %q{                          ~~~~~~ keyword (ensure)
        |       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression (ensure)
        |             ~~~~~~ keyword (ensure.rescue.resbody)
        |       ~~~~~~~~~~~~~~~~~ expression (ensure.rescue)})
  end

  def test_rescue_else_ensure
    assert_parses(
      s(:kwbegin,
        s(:ensure,
          s(:rescue,
            s(:send, nil, :meth),
            s(:resbody, nil, nil, s(:lvar, :baz)),
            s(:lvar, :foo)),
          s(:lvar, :bar))),
      %q{begin; meth; rescue; baz; else foo; ensure; bar end},
      %q{                                    ~~~~~~ keyword (ensure)
        |       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression (ensure)
        |             ~~~~~~ keyword (ensure.rescue.resbody)
        |                          ~~~~ else (ensure.rescue)
        |       ~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression (ensure.rescue)})
  end

  def test_rescue_mod
    assert_parses(
      s(:rescue,
        s(:send, nil, :meth),
        s(:resbody, nil, nil, s(:lvar, :bar)),
        nil),
      %q{meth rescue bar},
      %q{     ~~~~~~ keyword (resbody)
        |     ~~~~~~~~~~ expression (resbody)
        |~~~~~~~~~~~~~~~ expression})
  end

  def test_rescue_mod_asgn
    assert_parses(
      s(:lvasgn, :foo,
        s(:rescue,
          s(:send, nil, :meth),
          s(:resbody, nil, nil, s(:lvar, :bar)),
          nil)),
      %q{foo = meth rescue bar},
      %q{           ~~~~~~ keyword (rescue.resbody)
        |           ~~~~~~~~~~ expression (rescue.resbody)
        |      ~~~~~~~~~~~~~~~ expression (rescue)
        |~~~~~~~~~~~~~~~~~~~~~ expression})
  end

  def test_rescue_mod_op_assign
    assert_parses(
      s(:op_asgn,
        s(:lvasgn, :foo), :+,
        s(:rescue,
          s(:send, nil, :meth),
          s(:resbody, nil, nil, s(:lvar, :bar)),
          nil)),
      %q{foo += meth rescue bar},
      %q{            ~~~~~~ keyword (rescue.resbody)
        |            ~~~~~~~~~~ expression (rescue.resbody)
        |       ~~~~~~~~~~~~~~~ expression (rescue)
        |~~~~~~~~~~~~~~~~~~~~~~ expression},
      ALL_VERSIONS - %w(1.8))
  end

  def test_resbody_list
    assert_parses(
      s(:kwbegin,
        s(:rescue,
          s(:send, nil, :meth),
          s(:resbody,
            s(:array, s(:const, nil, :Exception)),
            nil,
            s(:lvar, :bar)),
          nil)),
      %q{begin; meth; rescue Exception; bar; end})
  end

  def test_resbody_list_mrhs
    assert_parses(
      s(:kwbegin,
        s(:rescue,
          s(:send, nil, :meth),
          s(:resbody,
            s(:array,
              s(:const, nil, :Exception),
              s(:lvar, :foo)),
            nil,
            s(:lvar, :bar)),
          nil)),
      %q{begin; meth; rescue Exception, foo; bar; end})
  end

  def test_resbody_var
    assert_parses(
      s(:kwbegin,
        s(:rescue,
          s(:send, nil, :meth),
          s(:resbody, nil, s(:lvasgn, :ex), s(:lvar, :bar)),
          nil)),
      %q{begin; meth; rescue => ex; bar; end})

    assert_parses(
      s(:kwbegin,
        s(:rescue,
          s(:send, nil, :meth),
          s(:resbody, nil, s(:ivasgn, :@ex), s(:lvar, :bar)),
          nil)),
      %q{begin; meth; rescue => @ex; bar; end})
  end

  def test_resbody_list_var
    assert_parses(
      s(:kwbegin,
        s(:rescue,
          s(:send, nil, :meth),
          s(:resbody,
            s(:array, s(:lvar, :foo)),
            s(:lvasgn, :ex),
            s(:lvar, :bar)),
          nil)),
      %q{begin; meth; rescue foo => ex; bar; end})
  end

  def test_retry
    assert_parses(
      s(:retry),
      %q{retry},
      %q{~~~~~ keyword
        |~~~~~ expression})
  end

  # BEGIN and END

  def test_preexe
    assert_parses(
      s(:preexe, s(:int, 1)),
      %q{BEGIN { 1 }},
      %q{~~~~~ keyword
        |      ^ begin
        |          ^ end
        |~~~~~~~~~~~ expression})
  end

  def test_preexe_invalid
    assert_diagnoses(
      [:error, :begin_in_method],
      %q{def f; BEGIN{}; end},
      %q{       ~~~~~ location},
      # Yes. *Exclude 1.9*. Sigh.
      ALL_VERSIONS - %w(1.9))
  end

  def test_postexe
    assert_parses(
      s(:postexe, s(:int, 1)),
      %q{END { 1 }},
      %q{~~~ keyword
        |    ^ begin
        |        ^ end
        |~~~~~~~~~ expression})
  end

  #
  # Miscellanea
  #

  def test_kwbegin_compstmt
    assert_parses(
      s(:kwbegin,
        s(:send, nil, :foo!),
        s(:send, nil, :bar!)),
      %q{begin foo!; bar! end})
  end

  def test_crlf_line_endings
    with_versions(ALL_VERSIONS) do |_ver, parser|
      source_file = Parser::Source::Buffer.new('(comments)')
      source_file.source = "\r\nfoo"

      range = lambda do |from, to|
        Parser::Source::Range.new(source_file, from, to)
      end

      ast = parser.parse(source_file)

      assert_equal s(:lvar, :foo),
                   ast

      assert_equal range.call(1, 4),
                   ast.loc.expression
    end
  end

  def test_begin_cmdarg
    assert_parses(
      s(:send, nil, :p,
        s(:kwbegin,
          s(:block,
            s(:send, s(:int, 1), :times),
            s(:args),
            s(:int, 1)))),
      %q{p begin 1.times do 1 end end},
      %{},
      ALL_VERSIONS - %w(1.8 1.9))
  end

  def test_bug_cmdarg
    assert_parses(
      s(:send, nil, :meth,
        s(:begin,
          s(:block,
            s(:send, nil, :lambda),
            s(:args), nil))),
      %q{meth (lambda do end)},
      %q{},
      %w(1.8))

    assert_parses(
      s(:send, nil, :assert,
        s(:send, nil, :dogs)),
      %q{assert dogs})

    assert_parses(
      s(:send, nil, :assert,
        s(:hash,
          s(:pair, s(:sym, :do), s(:true)))),
      %q{assert do: true},
      %q{},
      ALL_VERSIONS - %w(1.8))

    assert_parses(
      s(:send, nil, :f,
        s(:hash,
          s(:pair,
            s(:sym, :x),
            s(:block,
              s(:send, nil, :lambda),
              s(:args),
              s(:block,
                s(:send, nil, :meth),
                s(:args), nil))))),
      %q{f x: -> do meth do end end},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_file_line_non_literals
    with_versions(ALL_VERSIONS) do |_ver, parser|
      parser.builder.emit_file_line_as_literals = false

      source_file = Parser::Source::Buffer.new('(comments)')
      source_file.source = "[__FILE__, __LINE__]"

      ast = parser.parse(source_file)

      assert_equal s(:array, s(:__FILE__), s(:__LINE__)), ast
    end
  end

  if defined?(Encoding)
    def test_bom
      assert_parses(
        s(:int, 1),
        %Q{\xef\xbb\xbf1}.force_encoding(Encoding::BINARY),
        %q{},
        %w(1.9 2.0 2.1))
    end

    def test_magic_encoding_comment
      assert_parses(
        s(:begin,
          s(:lvasgn, :"проверка", s(:int, 42)),
          s(:send, nil, :puts, s(:lvar, :"проверка"))),
        %Q{# coding:koi8-r
           \xd0\xd2\xcf\xd7\xc5\xd2\xcb\xc1 = 42
           puts \xd0\xd2\xcf\xd7\xc5\xd2\xcb\xc1}.
          force_encoding(Encoding::BINARY),
        %q{},
        %w(1.9 2.0 2.1))
    end

    def test_regexp_encoding
      assert_parses(
        s(:match_with_lvasgn,
          s(:regexp,
            s(:str, "\\xa8"),
            s(:regopt, :n)),
          s(:str, "")),
        %q{/\xa8/n =~ ""}.force_encoding(Encoding::UTF_8),
        %{},
        ALL_VERSIONS - %w(1.8))
    end
  end

  #
  # Error recovery
  #

  def test_unknown_percent_str
    assert_diagnoses(
      [:error, :unexpected_percent_str, { :type => '%k' }],
      %q{%k[foo]},
      %q{~~ location})
  end

  def test_unterminated_embedded_doc
    assert_diagnoses(
      [:fatal, :embedded_document],
      %Q{=begin\nfoo\nend},
      %q{~~~~~~ location})

    assert_diagnoses(
      [:fatal, :embedded_document],
      %Q{=begin\nfoo\nend\n},
      %q{~~~~~~ location})
  end

  def test_codepoint_too_large
    assert_diagnoses(
      [:error, :unicode_point_too_large],
      %q{"\u{120 120000}"},
      %q{        ~~~~~~ location},
      ALL_VERSIONS - %w(1.8))
  end

  def test_on_error
    assert_diagnoses(
      [:error, :unexpected_token, { :token => 'tIDENTIFIER' }],
      %q{def foo(bar baz); end},
      %q{            ~~~ location})
  end

  #
  # Token and comment extraction
  #

  def assert_parses_with_comments(ast_pattern, source, comments_pattern)
    with_versions(ALL_VERSIONS) do |_ver, parser|
      source_file = Parser::Source::Buffer.new('(comments)')
      source_file.source = source

      comments_pattern_here = comments_pattern.map do |(from, to)|
        range = Parser::Source::Range.new(source_file, from, to)
        Parser::Source::Comment.new(range)
      end

      ast, comments = parser.parse_with_comments(source_file)

      assert_equal ast_pattern, ast

      assert_equal comments_pattern_here, comments
    end
  end

  def test_comment_interleaved
    assert_parses_with_comments(
      s(:send, s(:int, 1), :+, s(:int, 2)),
      %Q{1 + # foo\n 2},
      [ [4, 9] ])
  end

  def test_comment_single
    assert_parses_with_comments(
      s(:send, nil, :puts),
      %Q{puts # whatever},
      [ [5, 15] ])
  end

  def test_tokenize
    with_versions(ALL_VERSIONS) do |_ver, parser|
      source_file = Parser::Source::Buffer.new('(tokenize)')
      source_file.source = "1 + # foo\n 2"

      range = lambda do |from, to|
        Parser::Source::Range.new(source_file, from, to)
      end

      ast, comments, tokens = parser.tokenize(source_file)

      assert_equal s(:send, s(:int, 1), :+, s(:int, 2)),
                   ast

      assert_equal [
                     Parser::Source::Comment.new(range.call(4, 9))
                   ], comments

      assert_equal [
                     [:tINTEGER, [ 1,       range.call(0, 1) ]],
                     [:tPLUS,    [ '+',     range.call(2, 3) ]],
                     [:tCOMMENT, [ '# foo', range.call(4, 9) ]],
                     [:tINTEGER, [ 2,       range.call(11, 12) ]],
                   ], tokens
    end
  end

  #
  # Bug-specific tests
  #

  def test_bug_cmd_string_lookahead
    assert_parses(
      s(:block,
        s(:send, nil, :desc,
          s(:str, 'foo')),
        s(:args), nil),
      %q{desc "foo" do end})
  end

  def test_bug_do_block_in_call_args
    # [ruby-core:59342] [Bug #9308]
    assert_parses(
      s(:send, nil, :bar,
        s(:def, :foo,
          s(:args),
          s(:block,
            s(:send, s(:self), :each),
            s(:args),
            nil))),
      %q{bar def foo; self.each do end end},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_bug_do_block_in_cmdarg
    # [ruby-core:61950] [Bug #9726]
    assert_parses(
      s(:send, nil, :tap,
        s(:begin,
          s(:block,
            s(:send, nil, :proc),
            s(:args), nil))),
      %q{tap (proc do end)},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_bug_interp_single
    assert_parses(
      s(:dstr, s(:begin, s(:int, 1))),
      %q{"#{1}"})

    assert_parses(
      s(:array, s(:dstr, s(:begin, s(:int, 1)))),
      %q{%W"#{1}"})
  end

  def test_bug_def_no_paren_eql_begin
    assert_parses(
      s(:def, :foo, s(:args), nil),
      %Q{def foo\n=begin\n=end\nend})
  end

  def test_bug_while_not_parens_do
    assert_parses(
      s(:while, s(:send, s(:begin, s(:true)), :"!"), nil),
      %q{while not (true) do end},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_bug_rescue_empty_else
    assert_parses(
      s(:kwbegin,
        s(:rescue, nil,
          s(:resbody,
            s(:array,
              s(:const, nil, :LoadError)), nil, nil), nil)),
      %q{begin; rescue LoadError; else; end},
      %q{                         ~~~~ else (rescue)
        |       ~~~~~~~~~~~~~~~~~~~~~~ expression (rescue)})
  end

  def test_bug_heredoc_do
    assert_parses(
      s(:block,
        s(:send, nil, :f,
          s(:dstr)),
        s(:args), nil),
      %Q{f <<-TABLE do\nTABLE\nend})
  end

  def test_ruby_bug_9669
    assert_parses(
      s(:def, :a, s(:args, s(:kwarg, :b)), s(:return)),
      %Q{def a b:\nreturn\nend},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9 2.0))

    assert_parses(
      s(:lvasgn, :o,
        s(:hash,
          s(:pair, s(:sym, :a), s(:int, 1)))),
      %Q{o = {\na:\n1\n}},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def test_ruby_bug_10279
    assert_parses(
      s(:hash,
        s(:pair, s(:sym, :a),
        s(:if, s(:true), s(:int, 42), nil))),
      %q{{a: if true then 42 end}},
      %q{},
      ALL_VERSIONS - %w(1.8 1.9 2.0))
  end

  def test_bug_lambda_leakage
    assert_parses(
      s(:begin,
        s(:block,
          s(:send, nil, :lambda),
          s(:args,
            s(:arg, :scope)), nil),
        s(:send, nil, :scope)),
      %q{->(scope) {}; scope},
      %q{},
      ALL_VERSIONS - %w(1.8))
  end

  def test_bug_regex_verification
    assert_parses(
      s(:regexp, s(:str, "#)"), s(:regopt, :x)),
      %Q{/#)/x})
  end
end
