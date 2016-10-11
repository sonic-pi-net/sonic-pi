require "test_helper"

class SomeClassTest < Test::Unit::TestCase
  def setup
    @instance = SomeClass.new("foo")
  end

  def test_reverse
    assert_equal "oof", @instance.reverse
  end

  def test_comparison
    assert @instance.compare_with("foo")
  end
end
