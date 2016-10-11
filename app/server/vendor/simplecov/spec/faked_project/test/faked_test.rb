require "test_helper"

class FakedTest < Test::Unit::TestCase
  def test_something
    assert_equal "bar", FakedProject.foo
  end

  def test_framework_specific
    assert_equal "Only tested in Test/Unit", FrameworkSpecific.test_unit
  end
end
