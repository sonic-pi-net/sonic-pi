require File.expand_path('../../../test_helper', __FILE__)

require 'mocha/parameter_matchers/includes'
require 'mocha/inspect'

class IncludesTest < Mocha::TestCase

  include Mocha::ParameterMatchers

  def test_should_match_object_including_value
    matcher = includes(:x)
    assert matcher.matches?([[:x, :y, :z]])
  end

  def test_should_match_object_including_all_values
    matcher = includes(:x, :y, :z)
    assert matcher.matches?([[:x, :y, :z]])
  end

  def test_should_not_match_object_that_does_not_include_value
    matcher = includes(:not_included)
    assert !matcher.matches?([[:x, :y, :z]])
  end

  def test_should_not_match_object_that_does_not_include_any_one_value
    matcher = includes(:x, :y, :z, :not_included)
    assert !matcher.matches?([[:x, :y, :z]])
  end

  def test_should_describe_matcher_with_one_item
    matcher = includes(:x)
    assert_equal "includes(:x)", matcher.mocha_inspect
  end

  def test_should_describe_matcher_with_multiple_items
    matcher = includes(:x, :y, :z)
    assert_equal "includes(:x, :y, :z)", matcher.mocha_inspect
  end

  def test_should_not_raise_error_on_emtpy_arguments
    matcher = includes(:x)
    assert_nothing_raised { matcher.matches?([]) }
  end

  def test_should_not_match_on_empty_arguments
    matcher = includes(:x)
    assert !matcher.matches?([])
  end

  def test_should_not_raise_error_on_argument_that_does_not_respond_to_include
    matcher = includes(:x)
    assert_nothing_raised { matcher.matches?([:x]) }
  end

  def test_should_not_match_on_argument_that_does_not_respond_to_include
    matcher = includes(:x)
    assert !matcher.matches?([:x])
  end
end
