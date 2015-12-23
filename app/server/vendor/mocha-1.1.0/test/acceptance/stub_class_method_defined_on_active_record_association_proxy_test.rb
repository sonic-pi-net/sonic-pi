require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class StubClassMethodDefinedOnActiveRecordAssociationProxyTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_be_able_to_stub_method_if_ruby18_public_methods_include_method_but_method_does_not_actually_exist_like_active_record_association_proxy
    ruby18_klass = Class.new do
      class << self
        def public_methods(include_superclass = true)
          ['my_class_method']
        end
      end
    end
    test_result = run_as_test do
      ruby18_klass.stubs(:my_class_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby18_klass.my_class_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby19_public_methods_include_method_but_method_does_not_actually_exist_like_active_record_association_proxy
    ruby19_klass = Class.new do
      class << self
        def public_methods(include_superclass = true)
          [:my_class_method]
        end
      end
    end
    test_result = run_as_test do
      ruby19_klass.stubs(:my_class_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby19_klass.my_class_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby18_protected_methods_include_method_but_method_does_not_actually_exist_like_active_record_association_proxy
    ruby18_klass = Class.new do
      class << self
        def protected_methods(include_superclass = true)
          ['my_class_method']
        end
      end
    end
    test_result = run_as_test do
      ruby18_klass.stubs(:my_class_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby18_klass.my_class_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby19_protected_methods_include_method_but_method_does_not_actually_exist_like_active_record_association_proxy
    ruby19_klass = Class.new do
      class << self
        def protected_methods(include_superclass = true)
          [:my_class_method]
        end
      end
    end
    test_result = run_as_test do
      ruby19_klass.stubs(:my_class_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby19_klass.my_class_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby18_private_methods_include_method_but_method_does_not_actually_exist_like_active_record_association_proxy
    ruby18_klass = Class.new do
      class << self
        def private_methods(include_superclass = true)
          ['my_class_method']
        end
      end
    end
    test_result = run_as_test do
      ruby18_klass.stubs(:my_class_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby18_klass.my_class_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby19_private_methods_include_method_but_method_does_not_actually_exist_like_active_record_association_proxy
    ruby19_klass = Class.new do
      class << self
        def private_methods(include_superclass = true)
          [:my_class_method]
        end
      end
    end
    test_result = run_as_test do
      ruby19_klass.stubs(:my_class_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby19_klass.my_class_method
    end
    assert_passed(test_result)
  end

end
