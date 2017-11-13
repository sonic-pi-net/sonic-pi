require File.expand_path('../../test_helper', __FILE__)
require 'method_definer'
require 'mocha/mock'

require 'mocha/class_method'

class ClassMethodTest < Mocha::TestCase

  include Mocha

  def test_should_hide_original_method
    klass = Class.new { def self.method_x; end }
    method = ClassMethod.new(klass, :method_x)

    method.hide_original_method

    assert_equal false, klass.respond_to?(:method_x)
  end

  def test_should_not_raise_error_hiding_method_that_isnt_defined
    klass = Class.new
    method = ClassMethod.new(klass, :method_x)

    assert_nothing_raised { method.hide_original_method }
  end

  def test_should_not_raise_error_hiding_method_in_class_that_implements_method_called_method
    klass = Class.new { def self.method; end }
    method = ClassMethod.new(klass, :method)

    assert_nothing_raised { method.hide_original_method }
  end

  def test_should_define_a_new_method_which_should_call_mocha_method_missing
    klass = Class.new { def self.method_x; end }
    mocha = build_mock
    klass.define_instance_method(:mocha) { mocha }
    mocha.expects(:method_x).with(:param1, :param2).returns(:result)
    method = ClassMethod.new(klass, :method_x)

    method.hide_original_method
    method.define_new_method
    result = klass.method_x(:param1, :param2)

    assert_equal :result, result
    assert mocha.__verified__?
  end

  def test_should_remove_new_method
    klass = Class.new { def self.method_x; end }
    method = ClassMethod.new(klass, :method_x)

    method.remove_new_method

    assert_equal false, klass.respond_to?(:method_x)
  end

  def test_should_restore_original_method
    klass = Class.new { def self.method_x; :original_result; end }
    method = ClassMethod.new(klass, :method_x)

    method.hide_original_method
    method.define_new_method
    method.remove_new_method
    method.restore_original_method

    assert klass.respond_to?(:method_x)
    assert_equal :original_result, klass.method_x
  end

  def test_should_restore_original_method_accepting_a_block_parameter
    klass = Class.new { def self.method_x(&block); block.call if block_given? ; end }
    method = ClassMethod.new(klass, :method_x)

    method.hide_original_method
    method.define_new_method
    method.remove_new_method
    method.restore_original_method

    block_called = false
    klass.method_x { block_called = true }
    assert block_called
  end

  def test_should_not_restore_original_method_if_none_was_defined_in_first_place
    klass = Class.new { def self.method_x; :new_result; end }
    method = ClassMethod.new(klass, :method_x)

    method.restore_original_method

    assert_equal :new_result, klass.method_x
  end

  def test_should_call_hide_original_method
    klass = Class.new { def self.method_x; end }
    method = ClassMethod.new(klass, :method_x)
    method.hide_original_method
    method.define_instance_accessor(:hide_called)
    method.replace_instance_method(:hide_original_method) { self.hide_called = true }

    method.stub

    assert method.hide_called
  end

  def test_should_call_define_new_method
    klass = Class.new { def self.method_x; end }
    method = ClassMethod.new(klass, :method_x)
    method.define_instance_accessor(:define_called)
    method.replace_instance_method(:define_new_method) { self.define_called = true }

    method.stub

    assert method.define_called
  end

  def test_should_call_remove_new_method
    klass = Class.new { def self.method_x; end }
    method = ClassMethod.new(klass, :method_x)
    mocha = build_mock
    klass.define_instance_method(:mocha) { mocha }
    method.define_instance_accessor(:remove_called)
    method.replace_instance_method(:remove_new_method) { self.remove_called = true }

    method.unstub

    assert method.remove_called
  end

  def test_should_call_restore_original_method
    klass = Class.new { def self.method_x; end }
    mocha = build_mock
    klass.define_instance_method(:mocha) { mocha }
    method = ClassMethod.new(klass, :method_x)
    method.define_instance_accessor(:restore_called)
    method.replace_instance_method(:restore_original_method) { self.restore_called = true }

    method.unstub

    assert method.restore_called
  end

  def test_should_call_mocha_unstub
    klass = Class.new { def self.method_x; end }
    method = ClassMethod.new(klass, :method_x)
    method.replace_instance_method(:restore_original_method) { }
    mocha = Class.new { class << self; attr_accessor :unstub_method; end; def self.unstub(method); self.unstub_method = method; end; }
    mocha.define_instance_method(:any_expectations?) { true }
    method.replace_instance_method(:mock) { mocha }

    method.unstub
    assert_equal mocha.unstub_method, :method_x
  end

  def test_should_call_stubbee_reset_mocha_if_no_expectations_remaining
    klass = Class.new { def self.method_x; end }
    method = ClassMethod.new(klass, :method_x)
    method.replace_instance_method(:remove_new_method) { }
    method.replace_instance_method(:restore_original_method) { }
    mocha = Class.new
    mocha.define_instance_method(:unstub) { |method_name| }
    mocha.define_instance_method(:any_expectations?) { false }
    method.replace_instance_method(:mock) { mocha }
    stubbee = Class.new { attr_accessor :reset_mocha_called; def reset_mocha; self.reset_mocha_called = true; end; }.new
    method.replace_instance_method(:stubbee) { stubbee }

    method.unstub

    assert stubbee.reset_mocha_called
  end

  def test_should_return_mock_for_stubbee
    mocha = Object.new
    stubbee = Object.new
    stubbee.define_instance_accessor(:mocha) { mocha }
    stubbee.mocha = nil
    method = ClassMethod.new(stubbee, :method_name)
    assert_equal stubbee.mocha, method.mock
  end

  def test_should_not_match_if_other_object_has_a_different_class
    class_method = ClassMethod.new(Object.new, :method)
    other_object = Object.new
    assert !class_method.matches?(other_object)
  end

  def test_should_not_match_if_other_class_method_has_different_stubbee
    stubbee_1 = Object.new
    stubbee_2 = Object.new
    class_method_1 = ClassMethod.new(stubbee_1, :method)
    class_method_2 = ClassMethod.new(stubbee_2, :method)
    assert !class_method_1.matches?(class_method_2)
  end

  def test_should_not_match_if_other_class_method_has_different_method
    stubbee = Object.new
    class_method_1 = ClassMethod.new(stubbee, :method_1)
    class_method_2 = ClassMethod.new(stubbee, :method_2)
    assert !class_method_1.matches?(class_method_2)
  end

  def test_should_match_if_other_class_method_has_same_stubbee_and_same_method_so_no_attempt_is_made_to_stub_a_method_twice
    stubbee = Object.new
    class_method_1 = ClassMethod.new(stubbee, :method)
    class_method_2 = ClassMethod.new(stubbee, :method)
    assert class_method_1.matches?(class_method_2)
  end

  def test_should_match_if_other_class_method_has_same_stubbee_and_same_method_but_stubbee_equal_method_lies_like_active_record_association_proxy
    stubbee = Class.new do
      def equal?(other); false; end
    end.new
    class_method_1 = ClassMethod.new(stubbee, :method)
    class_method_2 = ClassMethod.new(stubbee, :method)
    assert class_method_1.matches?(class_method_2)
  end

  private

  def build_mock
    Mock.new(nil)
  end
end
