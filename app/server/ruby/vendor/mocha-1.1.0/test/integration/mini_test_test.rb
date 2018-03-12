require File.expand_path('../../test_helper', __FILE__)

require "mocha/mini_test"
require "integration/shared_tests"

class MiniTestTest < Mocha::TestCase
  include SharedTests
end
