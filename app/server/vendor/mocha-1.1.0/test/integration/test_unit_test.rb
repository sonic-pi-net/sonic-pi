require File.expand_path('../../test_helper', __FILE__)

require "mocha/test_unit"
require "integration/shared_tests"

class TestUnitTest < Mocha::TestCase
  include SharedTests
end
