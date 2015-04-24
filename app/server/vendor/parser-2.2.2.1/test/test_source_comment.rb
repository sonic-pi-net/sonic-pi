require 'helper'

class TestSourceComment < Minitest::Test
  def setup
    @buf = Parser::Source::Buffer.new('(string)')
    @buf.source = "# foo\n=begin foo\nbar\n=end baz\n"
  end

  def range(s, e)
    Parser::Source::Range.new(@buf, s, e)
  end

  def test_initialize
    comment = Parser::Source::Comment.new(range(0, 5))
    assert comment.frozen?
  end

  def test_text
    comment = Parser::Source::Comment.new(range(0, 5))
    assert_equal '# foo', comment.text
  end

  def test_inline
    comment = Parser::Source::Comment.new(range(0, 5))
    assert_equal :inline, comment.type
    assert comment.inline?
  end

  def test_document
    comment = Parser::Source::Comment.new(range(6, 25))
    assert_equal :document, comment.type
    assert comment.document?
  end
end
