require 'helper'
require 'parser/current'

class TestBase < Minitest::Test
  include AST::Sexp

  def test_parse
    ast = Parser::CurrentRuby.parse('1')
    assert_equal s(:int, 1), ast
  end

  def test_parse_with_comments
    ast, comments = Parser::CurrentRuby.parse_with_comments('1 # foo')
    assert_equal s(:int, 1), ast
    assert_equal 1, comments.size
    assert_equal '# foo', comments.first.text
  end

  def test_loc_to_node
    ast = Parser::CurrentRuby.parse('1')
    assert_equal ast.loc.node, ast
  end

  def test_loc_dup
    ast = Parser::CurrentRuby.parse('1')
    assert_equal nil, ast.loc.dup.node
    Parser::AST::Node.new(:root, [], :location => ast.loc)
  end
end
