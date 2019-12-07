require 'mocha/integration/assertion_counter'
require 'mocha/integration/monkey_patcher'
require 'mocha/expectation_error'

module Mocha
  module Integration
    module TestUnit
      module GemVersion230To250
        def self.applicable_to?(test_unit_version, ruby_version = nil)
          Gem::Requirement.new('>= 2.3.0', '<= 2.5.0').satisfied_by?(test_unit_version)
        end

        def self.description
          "monkey patch for Test::Unit gem >= v2.3.0 and <= v2.5.0"
        end

        def self.included(mod)
          MonkeyPatcher.apply(mod, RunMethodPatch)
        end

        module RunMethodPatch
          def run(result)
            assertion_counter = AssertionCounter.new(self)
            begin
              @internal_data.test_started
              @_result = result
              yield(Test::Unit::TestCase::STARTED, name)
              yield(Test::Unit::TestCase::STARTED_OBJECT, self)
              begin
                begin
                  run_setup
                  run_test
                  run_cleanup
                  mocha_verify(assertion_counter)
                  add_pass
                rescue Mocha::ExpectationError => e
                  add_failure(e.message, e.backtrace)
                rescue Exception
                  @internal_data.interrupted
                  raise unless handle_exception($!)
                ensure
                  begin
                    run_teardown
                  rescue Mocha::ExpectationError => e
                    add_failure(e.message, e.backtrace)
                  rescue Exception
                    raise unless handle_exception($!)
                  end
                end
              ensure
                mocha_teardown
              end
              @internal_data.test_finished
              result.add_run
              yield(Test::Unit::TestCase::FINISHED, name)
              yield(Test::Unit::TestCase::FINISHED_OBJECT, self)
            ensure
              # @_result = nil # For test-spec's after_all :<
            end
          end
        end
      end
    end
  end
end
