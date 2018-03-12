module Mocha
  module Detection
    module MiniTest
      def self.testcase
        if defined?(::Minitest::Test)
          ::Minitest::Test
        elsif defined?(::MiniTest::Unit::TestCase)
          ::MiniTest::Unit::TestCase
        else
          nil
        end
      end

      def self.version
        if defined?(::MiniTest::Unit::VERSION)
          ::MiniTest::Unit::VERSION
        elsif defined?(::Minitest::VERSION)
          ::Minitest::VERSION
        else
          '0.0.0'
        end
      end
    end
  end
end
