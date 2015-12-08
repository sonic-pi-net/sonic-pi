module DidYouMean
  module TestHelper
    def assert_suggestion(expected, array)
      assert_equal [expected], array, "Expected #{array.inspect} to only include #{expected.inspect}"
    end
  end
end
