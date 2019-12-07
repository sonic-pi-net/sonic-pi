require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class StubInstanceMethodDefinedOnActiveRecordAssociationProxyTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  def test_should_be_able_to_stub_method_if_ruby18_public_methods_include_method_but_method_does_not_exist
    ruby18_instance = Class.new do
      def public_methods(include_superclass = true)
        ['my_instance_method']
      end
    end.new
    test_result = run_as_test do
      ruby18_instance.stubs(:my_instance_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby18_instance.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby19_public_methods_include_method_but_method_does_not_exist
    ruby19_instance = Class.new do
      def public_methods(include_superclass = true)
        [:my_instance_method]
      end
    end.new
    test_result = run_as_test do
      ruby19_instance.stubs(:my_instance_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby19_instance.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby18_protected_methods_include_method_but_method_does_not_exist
    ruby18_instance = Class.new do
      def protected_methods(include_superclass = true)
        ['my_instance_method']
      end
    end.new
    test_result = run_as_test do
      ruby18_instance.stubs(:my_instance_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby18_instance.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby19_protected_methods_include_method_but_method_does_not_exist
    ruby19_instance = Class.new do
      def protected_methods(include_superclass = true)
        [:my_instance_method]
      end
    end.new
    test_result = run_as_test do
      ruby19_instance.stubs(:my_instance_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby19_instance.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby18_private_methods_include_method_but_method_does_not_exist
    ruby18_instance = Class.new do
      def private_methods(include_superclass = true)
        ['my_instance_method']
      end
    end.new
    test_result = run_as_test do
      ruby18_instance.stubs(:my_instance_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby18_instance.my_instance_method
    end
    assert_passed(test_result)
  end

  def test_should_be_able_to_stub_method_if_ruby19_private_methods_include_method_but_method_does_not_exist
    ruby19_instance = Class.new do
      def private_methods(include_superclass = true)
        [:my_instance_method]
      end
    end.new
    test_result = run_as_test do
      ruby19_instance.stubs(:my_instance_method).returns(:new_return_value)
      assert_equal :new_return_value, ruby19_instance.my_instance_method
    end
    assert_passed(test_result)
  end
end
