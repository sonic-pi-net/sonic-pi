require 'helper'

class TestStaticEnvironment < Minitest::Test
  def setup
    @env = Parser::StaticEnvironment.new
  end

  def test_declare
    refute @env.declared?(:foo)

    @env.declare :foo

    assert @env.declared?(:foo)
  end

  def test_extend_static
    @env.declare :foo
    @env.extend_static
    @env.declare :bar

    refute @env.declared?(:foo)
    assert @env.declared?(:bar)
  end

  def test_extend_dynamic
    @env.declare :foo
    @env.extend_dynamic
    @env.declare :bar

    assert @env.declared?(:foo)
    assert @env.declared?(:bar)
  end

  def test_unextend
    @env.declare :foo
    @env.extend_dynamic
    @env.declare :bar
    @env.unextend

    assert @env.declared?(:foo)
    refute @env.declared?(:bar)
  end
end
