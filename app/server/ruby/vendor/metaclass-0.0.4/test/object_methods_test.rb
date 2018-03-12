require "test_helper"

class ObjectMethodsTest < Test::Unit::TestCase

  def setup
    @klass = Class.new
  end

  def test_define_method_on_only_one_instance_of_a_class
    instance = @klass.new
    assert_raises(NoMethodError) { instance.success? }

    instance.__metaclass__.class_eval { def success?; true; end }
    assert_nothing_raised(NoMethodError) { assert instance.success? }

    another_instance = @klass.new
    assert_raises(NoMethodError) { another_instance.success? }
  end

  def test_metaclass_ancestors
    instance = @klass.new
    assert instance.__metaclass__.ancestors.include?(Object)
    assert instance.__metaclass__.ancestors.include?(Kernel)
  end

  def test_metaclass_is_itself_a_class
    instance = @klass.new
    assert instance.__metaclass__.is_a?(Class)
  end

end