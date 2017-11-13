require File.expand_path('../acceptance_test_helper', __FILE__)
require 'mocha/setup'

class PrependTest < Mocha::TestCase

  include AcceptanceTest

  def setup
    setup_acceptance_test
  end

  def teardown
    teardown_acceptance_test
  end

  if RUBY_VERSION >= '2.0'

    module Mod1
      def my_method
        super + " World"
      end
    end

    module Mod2
      def my_method
        super + " Wide"
      end
    end

    class Klass1
      prepend Mod1
      prepend Mod2

      def my_method
        "Hello"
      end
    end

    class Klass2
      class << self
        prepend Mod1
        prepend Mod2

        def my_method
          "Hello"
        end
      end
    end

    def test_stubbing_any_instance_with_multiple_prepended_methods
      assert_snapshot_unchanged(Klass1) do
        test_result = run_as_test do
          Klass1.any_instance.stubs(:my_method).returns("Bye World")
          assert_equal "Bye World", Klass1.new.my_method
        end
        assert_passed(test_result)
      end
      assert_equal "Hello World Wide", Klass1.new.my_method
    end

    def test_stubbing_instance_with_multiple_prepended_methods
      object = Klass1.new

      assert_snapshot_unchanged(object) do
        test_result = run_as_test do
          object.stubs(:my_method).returns("Bye World")
          assert_equal "Bye World", object.my_method
          assert_equal "Hello World Wide", Klass1.new.my_method
        end
        assert_passed(test_result)
      end
      assert_equal "Hello World Wide", object.my_method
    end

    def test_stubbing_a_prepended_class_method
      assert_snapshot_unchanged(Klass2) do
        test_result = run_as_test do
          Klass2.stubs(:my_method).returns("Bye World")
          assert_equal "Bye World", Klass2.my_method
        end
        assert_passed(test_result)
      end
      assert_equal "Hello World Wide", Klass2.my_method
    end

  end

end
