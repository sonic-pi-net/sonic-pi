require File.expand_path('../../../test_helper', __FILE__)

require 'mocha/parameter_matchers/has_entry'
require 'mocha/parameter_matchers/object'
require 'mocha/parameter_matchers/equals'
require 'mocha/inspect'

class HasEntryTest < Mocha::TestCase

  include Mocha::ParameterMatchers

  def test_should_match_hash_including_specified_key_value_pair
    matcher = has_entry(:key_1, 'value_1')
    assert matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_not_match_hash_not_including_specified_key_value_pair
    matcher = has_entry(:key_1, 'value_2')
    assert !matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_match_hash_including_specified_entry
    matcher = has_entry(:key_1 => 'value_1')
    assert matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_not_match_hash_not_including_specified_entry
    matcher = has_entry(:key_1 => 'value_2')
    assert !matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_describe_matcher_with_key_value_pair
    matcher = has_entry(:key_1, 'value_1')
    assert_equal "has_entry(:key_1 => 'value_1')", matcher.mocha_inspect
  end

  def test_should_describe_matcher_with_entry
    matcher = has_entry(:key_1 => 'value_1')
    assert_equal "has_entry(:key_1 => 'value_1')", matcher.mocha_inspect
  end

  def test_should_match_hash_including_specified_entry_with_nested_key_matcher
    matcher = has_entry(equals(:key_1) => 'value_1')
    assert matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_match_hash_including_specified_entry_with_nested_value_matcher
    matcher = has_entry(:key_1 => equals('value_1'))
    assert matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_not_match_hash_not_including_specified_entry_with_nested_key_matcher
    matcher = has_entry(equals(:key_1) => 'value_2')
    assert !matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_not_match_hash_not_including_specified_entry_with_nested_value_matcher
    matcher = has_entry(:key_1 => equals('value_2'))
    assert !matcher.matches?([{ :key_1 => 'value_1', :key_2 => 'value_2' }])
  end

  def test_should_not_match_object_that_doesnt_respond_to_keys
    matcher = has_entry(:key_1 => equals('value_2'))
    object = Class.new do
      def [](key)
        'value_2'
      end
    end.new
    assert !matcher.matches?([object])
  end

  def test_should_not_match_object_that_doesnt_respond_to_square_bracket
    matcher = has_entry(:key_1 => equals('value_2'))
    object = Class.new do
      def keys
        [:key_1]
      end
    end.new
    assert !matcher.matches?([object])
  end

  def test_should_raise_argument_error_if_single_argument_is_not_a_hash
    e = assert_raises(ArgumentError) do
      has_entry(Array.new)
    end
    assert_equal "Argument is not a Hash.", e.message
  end

  def test_should_raise_argument_error_if_no_entries_are_supplied
    e = assert_raises(ArgumentError) do
      has_entry({})
    end
    assert_equal "Argument has no entries.", e.message
  end

  def test_should_raise_argument_error_if_multiple_entries_are_supplied
    e = assert_raises(ArgumentError) do
      has_entry(:key_1 => 'value_1', :key_2 => 'value_2')
    end
    assert_equal "Argument has multiple entries. Use Mocha::ParameterMatchers#has_entries instead.", e.message
  end

  def test_should_raise_argument_error_if_more_than_two_arguments_are_supplied
    e = assert_raises(ArgumentError) do
      has_entry(1, 2, 3)
    end
    assert_equal "Too many arguments; use either a single argument (must be a Hash) or two arguments (a key and a value).", e.message
  end

  def test_should_match_array_as_key
    matcher = has_entry([:key_1, :key_2] => 'value_1')
    assert matcher.matches?([{[:key_1, :key_2] => 'value_1', :key_3 => 'value_2'}])
  end

  def test_should_match_array_as_value
    matcher = has_entry(:key_1 => ['value_1', 'value_2'])
    assert matcher.matches?([{:key_1 => ['value_1', 'value_2']}])
  end

  def test_should_match_hash_as_value_and_key
    matcher = has_entry({{:key_1 => 'value_1', :key_2 => 'value_2'} => {:key_3 => 'value_3', :key_4 => 'value_4'}})
    assert matcher.matches?([{{:key_1 => 'value_1', :key_2 => 'value_2'} => {:key_3 => 'value_3', :key_4 => 'value_4'}, :key_5 => 'value_5'}])
  end

  def test_should_match_matcher_as_value_and_key
    matcher = has_entry({has_entry(:key_1 => 'value_1') => has_entry(:key_3 => 'value_3')})
    assert matcher.matches?([{{:key_1 => 'value_1', :key_2 => 'value_2'} => {:key_3 => 'value_3', :key_4 => 'value_4'}, :key_5 => 'value_5'}])
  end
end
