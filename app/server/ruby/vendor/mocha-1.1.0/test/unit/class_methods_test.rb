require File.expand_path('../../test_helper', __FILE__)
require 'mocha/class_methods'
require 'mocha/object_methods'

class ClassMethodsTest < Mocha::TestCase

  def setup
    @klass = Class.new.extend(Mocha::ClassMethods, Mocha::ObjectMethods)
  end

  def test_should_build_any_instance_object
    any_instance = @klass.any_instance
    assert_not_nil any_instance
    assert any_instance.is_a?(Mocha::ClassMethods::AnyInstance)
  end

  def test_should_return_same_any_instance_object
    any_instance_1 = @klass.any_instance
    any_instance_2 = @klass.any_instance
    assert_equal any_instance_1, any_instance_2
  end

  def test_should_use_stubba_class_method_for_class
    assert_equal Mocha::ClassMethod, @klass.stubba_method
  end

  def test_should_use_stubba_class_method_for_any_instance
    assert_equal Mocha::AnyInstanceMethod, @klass.any_instance.stubba_method
  end

  def test_should_stub_self_for_class
    assert_equal @klass, @klass.stubba_object
  end

  def test_should_stub_relevant_class_for_any_instance
    any_instance = @klass.any_instance
    assert_equal @klass, any_instance.stubba_object
  end

end
