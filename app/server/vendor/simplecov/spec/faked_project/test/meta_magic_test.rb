require "test_helper"

class MetaMagicTest < Test::Unit::TestCase
  def test_class_methods
    assert_equal "this is a mixed-in class method", FakedProject.a_class_method
  end

  def test_instance_methods
    p = FakedProject.new
    assert_equal "this is a mixed-in instance method", p.an_instance_method
    assert_equal "A dynamically defined instance method", p.dynamic
  end
end
