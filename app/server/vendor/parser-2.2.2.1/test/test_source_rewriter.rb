require 'helper'

class TestSourceRewriter < Minitest::Test
  def setup
    @buf = Parser::Source::Buffer.new('(rewriter)')
    @buf.source = 'foo bar baz'

    @rewriter = Parser::Source::Rewriter.new(@buf)
  end

  def range(from, len)
    Parser::Source::Range.new(@buf, from, from + len)
  end

  def test_remove
    assert_equal 'foo  baz',
                 @rewriter.
                    remove(range(4, 3)).
                    process
  end

  def test_insert_before
    assert_equal 'foo quux bar baz',
                 @rewriter.
                    insert_before(range(4, 3), 'quux ').
                    process
  end

  def test_insert_after
    assert_equal 'foo bar quux baz',
                 @rewriter.
                    insert_after(range(4, 3), ' quux').
                    process
  end

  def test_replace
    assert_equal 'foo quux baz',
                 @rewriter.
                    replace(range(4, 3), 'quux').
                    process
  end

  def test_composing_asc
    assert_equal 'foo---bar---baz',
                 @rewriter.
                    replace(range(3, 1), '---').
                    replace(range(7, 1), '---').
                    process
  end

  def test_composing_desc
    assert_equal 'foo---bar---baz',
                 @rewriter.
                    replace(range(7, 1), '---').
                    replace(range(3, 1), '---').
                    process
  end

  def test_multiple_insertions_at_same_location
    assert_equal '<([foo] bar) baz>',
                 @rewriter.
                   insert_before(range(0, 11), '<').
                   insert_after( range(0, 11), '>').
                   insert_before(range(0, 7), '(').
                   insert_after( range(0, 7), ')').
                   insert_before(range(0, 3), '[').
                   insert_after( range(0, 3), ']').
                   process
  end

  def test_clobber
    diagnostics = []
    @rewriter.diagnostics.consumer = lambda do |diag|
      diagnostics << diag
    end

    assert_raises Parser::ClobberingError do
      @rewriter.
        replace(range(3, 1), '---').
        remove(range(3, 1))
    end

    assert_equal 2, diagnostics.count

    assert_equal :error, diagnostics.first.level
    assert_equal 'cannot remove 1 character(s)',
                 diagnostics.first.message
    assert_equal range(3, 1), diagnostics.first.location

    assert_equal :note, diagnostics.last.level
    assert_equal "clobbered by: replace 1 character(s) with \"---\"",
                 diagnostics.last.message
    assert_equal range(3, 1), diagnostics.last.location
  end

  def test_clobbering_error_backward_compatibility
    silence_diagnostics

    rescued = false

    # We use begin..rescue..end here rather than #assert_raises
    # since #assert_raises expects exact error class.
    begin
      @rewriter.
        replace(range(3, 1), '---').
        remove(range(3, 1))
    rescue RuntimeError => error
      rescued = true if error.message.include?('clobber')
    end

    assert rescued
  end

  def test_transaction_returns_self
    assert_equal @rewriter, @rewriter.transaction {}
  end

  def test_transaction_commit
    silence_diagnostics

    # Original: 'foo bar baz'

    # Rewrite as 'foo BAR baz'
    @rewriter.replace(range(4, 3), 'BAR')

    # Rewrite as '( bar )'
    @rewriter.transaction do
      @rewriter.replace(range(0, 3), '(')
      @rewriter.replace(range(8, 3), ')')
    end

    @rewriter.replace(range(3, 1), '_')
    @rewriter.replace(range(7, 1), '_')

    assert_equal '(_BAR_)', @rewriter.process
  end

  def test_transaction_rollback
    silence_diagnostics

    # Original: 'foo bar baz'

    # Rewrite as 'foo bar BAZ'
    @rewriter.replace(range(8, 3), 'BAZ')

    assert_raises Parser::ClobberingError do
      # Trying to rewrite as '( bar )', but it fails
      @rewriter.transaction do
        @rewriter.replace(range(0, 3), '(')
        @rewriter.replace(range(8, 3), ')')
      end
    end

    @rewriter.replace(range(0, 3), 'FOO')

    assert_equal 'FOO bar BAZ', @rewriter.process
  end

  def test_nested_transaction_raises_error
    error = assert_raises RuntimeError do
      @rewriter.transaction do
        @rewriter.transaction do
        end
      end
    end

    assert_match /nested/i, error.message
  end

  def test_process_in_transaction_raises_error
    error = assert_raises RuntimeError do
      @rewriter.transaction do
        @rewriter.process
      end
    end

    assert_match /transaction/, error.message
  end

  def silence_diagnostics
    @rewriter.diagnostics.consumer = proc {}
  end
end
