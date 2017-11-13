require 'helper'

class TestSourceBuffer < Minitest::Test
  def setup
    @buffer = Parser::Source::Buffer.new('(string)')
  end

  def test_initialize
    buffer = Parser::Source::Buffer.new('(string)')
    assert_equal '(string)', buffer.name
    assert_equal 1, buffer.first_line

    buffer = Parser::Source::Buffer.new('(string)', 5)
    assert_equal 5, buffer.first_line
  end

  def test_source_setter
    @buffer.source = 'foo'
    assert_equal 'foo', @buffer.source

    assert @buffer.source.frozen?
  end

  def test_source_double_setter
    @buffer.source = 'foo'

    assert_raises(ArgumentError) do
      @buffer.source = 'bar'
    end
  end

  if defined?(Encoding)
    def test_source_setter_encoding_error
      error = assert_raises EncodingError do
        @buffer.source = [
          '# encoding: utf-8',
          "# \xf9"
        ].join("\n")
      end

      assert_match /invalid byte sequence in UTF\-8/, error.message
    end
  end

  def test_read
    tempfile = Tempfile.new('parser')
    tempfile.write('foobar')
    tempfile.flush

    buffer = Parser::Source::Buffer.new(tempfile.path)
    buffer.read
    assert_equal 'foobar', buffer.source

    assert buffer.source.frozen?
  end

  def test_uninitialized
    assert_raises RuntimeError do
      @buffer.source
    end
  end

  def test_decompose_position
    @buffer.source = "1\nfoo\nbar"

    assert_equal [1, 0], @buffer.decompose_position(0)
    assert_equal [1, 1], @buffer.decompose_position(1)
    assert_equal [2, 0], @buffer.decompose_position(2)
    assert_equal [3, 1], @buffer.decompose_position(7)
  end

  def test_decompose_position_mapped
    @buffer = Parser::Source::Buffer.new('(string)', 5)
    @buffer.source = "1\nfoo\nbar"

    assert_equal [5, 0], @buffer.decompose_position(0)
    assert_equal [6, 0], @buffer.decompose_position(2)
  end

  def test_line
    @buffer.source = "1\nfoo\nbar"

    assert_equal '1', @buffer.source_line(1)
    assert_equal 'foo', @buffer.source_line(2)
  end

  def test_line_mutate
    @buffer.source = "1\nfoo\nbar"

    assert_equal '1', @buffer.source_line(1)

    @buffer.source_line(1)[0] = '2'
    assert_equal '1', @buffer.source_line(1)
  end

  def test_line_mapped
    @buffer = Parser::Source::Buffer.new('(string)', 5)
    @buffer.source = "1\nfoo\nbar"

    assert_equal '1', @buffer.source_line(5)
    assert_equal 'foo', @buffer.source_line(6)
  end
end
