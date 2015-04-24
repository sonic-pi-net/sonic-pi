require 'helper'
require 'parse_helper'

class TestParseHelper < Minitest::Test
  include ParseHelper

  def test_parser_for_ruby_version
    assert_instance_of Parser::Ruby18,
                       parser_for_ruby_version('1.8')

    unless RUBY_VERSION == '1.8.7'
      assert_instance_of Parser::Ruby19,
                         parser_for_ruby_version('1.9')

      assert_instance_of Parser::Ruby20,
                         parser_for_ruby_version('2.0')

      assert_instance_of Parser::Ruby21,
                         parser_for_ruby_version('2.1')
    end
  end

  def parse_maps(what)
    parse_source_map_descriptions(what).to_a
  end

  def test_parse_mapsation_description
    assert_equal [[0, 4, 'expr', [], '~~~~ expr']],
                 parse_maps('~~~~ expr')

    assert_equal [[0, 4, 'expr', [], '^~~~ expr']],
                 parse_maps('^~~~ expr')

    assert_equal [[0, 4, 'expr', [], '^^^^ expr']],
                 parse_maps('^^^^ expr')

    assert_equal [[2, 3, 'op', [], '  ^ op']],
                 parse_maps('  ^ op')

    assert_equal [[2, 3, 'op', ['foo'], '  ~ op (foo)']],
                 parse_maps('  ~ op (foo)')

    assert_equal [[2, 4, 'op', ['foo', 'bar'], '  ~~ op (foo.bar)']],
                 parse_maps('  ~~ op (foo.bar)')

    assert_equal [[2, 4, 'op', ['foo/2', 'bar'], '  ~~ op (foo/2.bar)']],
                 parse_maps('  ~~ op (foo/2.bar)')

    assert_equal [[0, 4, 'expr', [], '~~~~ expr'],
                  [5, 7, 'op', ['str', 'e_h'], '     ~~ op (str.e_h)']],
                 parse_maps(%{
                            |~~~~ expr
                            |     ~~ op (str.e_h)
                            })
  end

  def test_traverse_ast
    ast = s(:send,
            s(:int, 1), :+,
            s(:dstr,
              s(:str, 'foo'),
              s(:int, 2),
              s(:int, 3)))

    assert_equal ast, traverse_ast(ast, %w())

    assert_equal s(:int, 1), traverse_ast(ast, %w(int))
    assert_equal nil, traverse_ast(ast, %w(str))

    assert_equal s(:str, 'foo'), traverse_ast(ast, %w(dstr str))
    assert_equal s(:int, 2), traverse_ast(ast, %w(dstr int))
    assert_equal s(:int, 2), traverse_ast(ast, %w(dstr int/1))
    assert_equal s(:int, 3), traverse_ast(ast, %w(dstr int/2))
    assert_equal nil, traverse_ast(ast, %w(dstr int/3))
  end

  def test_assert_parses
    # Someone more clever and/or motivated than me is going to test this.
  end
end
