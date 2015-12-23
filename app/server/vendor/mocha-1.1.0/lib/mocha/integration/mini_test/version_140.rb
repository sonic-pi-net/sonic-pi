require 'mocha/integration/assertion_counter'
require 'mocha/integration/monkey_patcher'
require 'mocha/integration/mini_test/exception_translation'

module Mocha
  module Integration
    module MiniTest
      module Version140
        def self.applicable_to?(mini_test_version)
          Gem::Requirement.new('1.4.0').satisfied_by?(mini_test_version)
        end

        def self.description
          "monkey patch for MiniTest gem v1.4.0"
        end

        def self.included(mod)
          MonkeyPatcher.apply(mod, RunMethodPatch)
        end

        module RunMethodPatch
          def run runner
            assertion_counter = AssertionCounter.new(self)
            result = '.'
            begin
              begin
                @passed = nil
                self.setup
                self.__send__ self.__name__
                mocha_verify(assertion_counter)
                @passed = true
              rescue Exception => e
                @passed = false
                result = runner.puke(self.class, self.__name__, Mocha::Integration::MiniTest.translate(e))
              ensure
                begin
                  self.teardown
                rescue Exception => e
                  result = runner.puke(self.class, self.__name__, Mocha::Integration::MiniTest.translate(e))
                end
              end
            ensure
              mocha_teardown
            end
            result
          end
        end
      end
    end
  end
end
