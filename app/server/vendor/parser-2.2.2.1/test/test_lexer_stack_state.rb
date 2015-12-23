require 'helper'

class TestLexerStackState < Minitest::Test
  def setup
    @state = Parser::Lexer::StackState.new('state')
  end

  def test_push
    refute @state.active?

    assert_equal true, @state.push(true)
    assert @state.active?

    assert_equal false, @state.push(false)
    refute @state.active?
  end

  def test_clear
    @state.push true
    @state.clear

    refute @state.active?
  end

  def test_pop
    @state.push(true)

    assert_equal true, @state.pop
    refute @state.active?
  end

  def test_pop_empty
    assert_equal false, @state.pop
    refute @state.active?
  end

  def test_lexpop_10
    @state.push(true)
    @state.push(false)

    assert_equal true, @state.lexpop
    assert_equal true, @state.pop
  end

  def test_lexpop_01
    @state.push(false)
    @state.push(true)

    assert_equal true, @state.lexpop
    assert_equal true, @state.pop
  end

  def test_lexpop_00
    @state.push(false)
    @state.push(false)

    assert_equal false, @state.lexpop
    assert_equal false, @state.pop
  end

  def test_dup
    @state.push(true)
    new_state = @state.dup

    assert_equal true, @state.pop
    assert_equal true, new_state.pop
  end

  def test_to_s
    @state.push(true)
    @state.push(false)
    @state.push(false)

    assert_equal '[100 <= state]', @state.to_s
  end
end
