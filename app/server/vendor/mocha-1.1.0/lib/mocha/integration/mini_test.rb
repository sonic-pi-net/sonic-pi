require 'mocha/debug'

require 'mocha/detection/mini_test'

require 'mocha/integration/mini_test/nothing'
require 'mocha/integration/mini_test/version_13'
require 'mocha/integration/mini_test/version_140'
require 'mocha/integration/mini_test/version_141'
require 'mocha/integration/mini_test/version_142_to_172'
require 'mocha/integration/mini_test/version_200'
require 'mocha/integration/mini_test/version_201_to_222'
require 'mocha/integration/mini_test/version_230_to_2101'
require 'mocha/integration/mini_test/version_2110_to_2111'
require 'mocha/integration/mini_test/version_2112_to_320'
require 'mocha/integration/mini_test/adapter'

module Mocha
  module Integration
    module MiniTest
      def self.activate
        return false unless Detection::MiniTest.testcase
        mini_test_version = Gem::Version.new(Detection::MiniTest.version)

        Debug.puts "Detected MiniTest version: #{mini_test_version}"

        integration_module = [
          MiniTest::Adapter,
          MiniTest::Version2112To320,
          MiniTest::Version2110To2111,
          MiniTest::Version230To2101,
          MiniTest::Version201To222,
          MiniTest::Version200,
          MiniTest::Version142To172,
          MiniTest::Version141,
          MiniTest::Version140,
          MiniTest::Version13,
          MiniTest::Nothing
        ].detect { |m| m.applicable_to?(mini_test_version) }

        target = Detection::MiniTest.testcase
        unless target < integration_module
          Debug.puts "Applying #{integration_module.description}"
          target.send(:include, integration_module)
        end
        true
      end
    end
  end
end
