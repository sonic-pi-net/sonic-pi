require File.expand_path('../../../test_helper', __FILE__)

require 'mocha/parameter_matchers/responds_with'
require 'mocha/parameter_matchers/object'
require 'mocha/inspect'

class RespondsWithTest < Mocha::TestCase

  include Mocha::ParameterMatchers

  def test_should_match_parameter_responding_with_expected_value
    matcher = responds_with(:upcase, 'FOO')
    assert matcher.matches?(['foo'])
  end

  def test_should_not_match_parameter_responding_with_unexpected_value
    matcher = responds_with(:upcase, 'FOO')
    assert !matcher.matches?(['bar'])
  end

  def test_should_match_parameter_responding_with_nested_responds_with_matcher
    matcher = responds_with(:foo, responds_with(:bar, 'baz'))
    object = Class.new { def foo; Class.new { def bar; 'baz'; end }.new; end }.new
    assert matcher.matches?([object])
  end

  def test_should_describe_matcher
    matcher = responds_with(:foo, :bar)
    assert_equal 'responds_with(:foo, :bar)', matcher.mocha_inspect
  end

end
