require File.expand_path('../../test_helper', __FILE__)

require 'mocha/thrower'

class ThrowerTest < Mocha::TestCase

  include Mocha

  def test_should_throw_tag
    thrower = Thrower.new(:tag)
    assert_throws(:tag) { thrower.evaluate }
  end

  def test_should_throw_tag_with_return_value
    thrower = Thrower.new(:tag, 'return-value')
    return_value = catch(:tag) { thrower.evaluate }
    assert_equal 'return-value', return_value
  end

end
