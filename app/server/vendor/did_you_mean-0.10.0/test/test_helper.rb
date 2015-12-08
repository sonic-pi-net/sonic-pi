NAME_ERROR = (___ rescue $!).class

require 'minitest/autorun'
require 'minitest/unit'
require 'did_you_mean'

begin
  MiniTest::Test
rescue NameError
  MiniTest::Test = MiniTest::Unit::TestCase
end

require 'did_you_mean/test_helper'
MiniTest::Test.send :include, DidYouMean::TestHelper
