$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), "..", "..", ".."))
require "lib/simplecov"
SimpleCov.start
require "test/unit"
class FooTest < Test::Unit::TestCase
  def test_foo
    assert true
  end
end
