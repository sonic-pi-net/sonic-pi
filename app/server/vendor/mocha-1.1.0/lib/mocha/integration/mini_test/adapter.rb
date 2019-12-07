require 'mocha/api'
require 'mocha/integration/assertion_counter'
require 'mocha/expectation_error_factory'

module Mocha
  module Integration
    module MiniTest

      # Integrates Mocha into recent versions of MiniTest.
      #
      # See the source code for an example of how to integrate Mocha into a test library.
      module Adapter
        include Mocha::API

        # @private
        def self.applicable_to?(mini_test_version)
          Gem::Requirement.new('>= 3.3.0').satisfied_by?(mini_test_version)
        end

        # @private
        def self.description
          "adapter for MiniTest gem >= v3.3.0"
        end

        # @private
        def self.included(mod)
          Mocha::ExpectationErrorFactory.exception_class = ::MiniTest::Assertion
        end

        # @private
        def before_setup
          mocha_setup
          super
        end

        # @private
        def before_teardown
          return unless passed?
          assertion_counter = Integration::AssertionCounter.new(self)
          mocha_verify(assertion_counter)
        ensure
          super
        end

        # @private
        def after_teardown
          super
          mocha_teardown
        end
      end
    end
  end
end

