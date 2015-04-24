require 'helper'

class TestDiagnostic < Minitest::Test
  def setup
    @buffer = Parser::Source::Buffer.new('(string)')
    @buffer.source = 'if (this is some bad code + bugs)'

    @range1 = Parser::Source::Range.new(@buffer, 0, 2) # if
    @range2 = Parser::Source::Range.new(@buffer, 4, 8) # this
  end

  def test_verifies_levels
    error = assert_raises ArgumentError do
      Parser::Diagnostic.new(:foobar, :escape_eof, {}, @range1)
    end

    assert_match /level/, error.message
  end

  def test_freezes
    string     = 'foo'
    highlights = [@range2]

    diag = Parser::Diagnostic.new(:error, :escape_eof, @range1, highlights)
    assert diag.frozen?
    assert diag.arguments.frozen?
    assert diag.highlights.frozen?

    refute string.frozen?
    refute highlights.frozen?
  end

  def test_render
    location = Parser::Source::Range.new(@buffer, 26, 27)

    highlights = [
      Parser::Source::Range.new(@buffer, 21, 25),
      Parser::Source::Range.new(@buffer, 28, 32)
    ]

    diag  = Parser::Diagnostic.new(:error, :unexpected, { :character => '+' },
                                   location, highlights)
    assert_equal([
      "(string):1:27: error: unexpected `+'",
      'if (this is some bad code + bugs)',
      '                     ~~~~ ^ ~~~~ '
    ], diag.render)
  end
end
