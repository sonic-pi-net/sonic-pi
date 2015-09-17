require File.expand_path('../../test_helper', __FILE__)
require 'mocha/expectation_list'
require 'mocha/expectation'
require 'set'
require 'method_definer'

class ExpectationListTest < Mocha::TestCase

  include Mocha

  def test_should_return_added_expectation
    expectation_list = ExpectationList.new
    expectation = Expectation.new(nil, :my_method)
    assert_same expectation, expectation_list.add(expectation)
  end

  def test_should_find_matching_expectation
    expectation_list = ExpectationList.new
    expectation1 = Expectation.new(nil, :my_method).with(:argument1, :argument2)
    expectation2 = Expectation.new(nil, :my_method).with(:argument3, :argument4)
    expectation_list.add(expectation1)
    expectation_list.add(expectation2)
    assert_same expectation1, expectation_list.match(:my_method, :argument1, :argument2)
  end

  def test_should_remove_all_expectations_matching_method_name
    expectation_list = ExpectationList.new
    expectation1 = Expectation.new(nil, :method_one).with(:argument1, :argument2)
    expectation2 = Expectation.new(nil, :method_one).with(:argument3, :argument4)
    expectation3 = Expectation.new(nil, :method_two)
    expectation_list.add(expectation1)
    expectation_list.add(expectation2)
    expectation_list.add(expectation3)
    expectation_list.remove_all_matching_method(:method_one)
    assert_nil expectation_list.match(:method_one, :argument1, :argument2)
    assert_nil expectation_list.match(:method_one, :argument3, :argument4)
    assert_same expectation3, expectation_list.match(:method_two)
  end

  def test_should_find_most_recent_matching_expectation
    expectation_list = ExpectationList.new
    expectation1 = Expectation.new(nil, :my_method).with(:argument1, :argument2)
    expectation2 = Expectation.new(nil, :my_method).with(:argument1, :argument2)
    expectation_list.add(expectation1)
    expectation_list.add(expectation2)
    assert_same expectation2, expectation_list.match(:my_method, :argument1, :argument2)
  end

  def test_should_find_matching_expectation_allowing_invocation
    expectation_list = ExpectationList.new
    expectation1 = Expectation.new(nil, :my_method).with(:argument1, :argument2)
    expectation2 = Expectation.new(nil, :my_method).with(:argument3, :argument4)
    expectation1.define_instance_method(:invocations_allowed?) { true }
    expectation2.define_instance_method(:invocations_allowed?) { true }
    expectation_list.add(expectation1)
    expectation_list.add(expectation2)
    assert_same expectation1, expectation_list.match_allowing_invocation(:my_method, :argument1, :argument2)
  end

  def test_should_find_most_recent_matching_expectation_allowing_invocation
    expectation_list = ExpectationList.new
    expectation1 = Expectation.new(nil, :my_method)
    expectation2 = Expectation.new(nil, :my_method)
    expectation1.define_instance_method(:invocations_allowed?) { true }
    expectation2.define_instance_method(:invocations_allowed?) { false }
    expectation_list.add(expectation1)
    expectation_list.add(expectation2)
    assert_same expectation1, expectation_list.match_allowing_invocation(:my_method)
  end

  def test_should_combine_two_expectation_lists_into_one
    expectation_list1 = ExpectationList.new
    expectation_list2 = ExpectationList.new
    expectation1 = Expectation.new(nil, :my_method)
    expectation2 = Expectation.new(nil, :my_method)
    expectation_list1.add(expectation1)
    expectation_list2.add(expectation2)
    expectation_list = expectation_list1 + expectation_list2
    assert_equal [expectation1, expectation2], expectation_list.to_a
  end

end
