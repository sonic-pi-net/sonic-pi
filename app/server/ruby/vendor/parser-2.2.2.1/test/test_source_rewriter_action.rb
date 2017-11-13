require 'helper'

class TestSourceRewriterAction < Minitest::Test
  def setup
    @buf = Parser::Source::Buffer.new('(rewriter_action)')
    @buf.source = 'foo bar baz'
  end

  def range(from, len)
    Parser::Source::Range.new(@buf, from, from + len)
  end

  def action(range, replacement)
    Parser::Source::Rewriter::Action.new(range, replacement)
  end

  def test_accessors
    action = action(range(1, 10), 'foo')

    assert action.frozen?
    assert_equal range(1, 10), action.range
    assert_equal 'foo',        action.replacement
  end

  def test_to_s_replace
    action = action(range(3, 1), 'foo')
    assert_equal "replace 1 character(s) with \"foo\"", action.to_s
  end

  def test_to_s_insert
    action = action(range(3, 0), 'foo')
    assert_equal "insert \"foo\"", action.to_s
  end

  def test_to_s_remove
    action = action(range(3, 2), '')
    assert_equal 'remove 2 character(s)', action.to_s
  end

  def test_to_s_nop
    action = action(range(3, 0), '')
    assert_equal 'do nothing', action.to_s
  end
end
