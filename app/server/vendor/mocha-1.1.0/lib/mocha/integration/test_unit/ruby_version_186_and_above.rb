require 'mocha/integration/assertion_counter'
require 'mocha/integration/monkey_patcher'
require 'mocha/expectation_error'

module Mocha
  module Integration
    module TestUnit
      module RubyVersion186AndAbove
        def self.applicable_to?(test_unit_version, ruby_version)
          Gem::Requirement.new('<= 1.2.3').satisfied_by?(test_unit_version) && Gem::Requirement.new('>= 1.8.6').satisfied_by?(ruby_version)
        end

        def self.description
          "monkey patch for standard library Test::Unit in Ruby >= v1.8.6"
        end

        def self.included(mod)
          MonkeyPatcher.apply(mod, RunMethodPatch)
        end

        module RunMethodPatch
          def run(result)
            assertion_counter = AssertionCounter.new(self)
            yield(Test::Unit::TestCase::STARTED, name)
            @_result = result
            begin
              begin
                setup
                __send__(@method_name)
                mocha_verify(assertion_counter)
              rescue Mocha::ExpectationError => e
                add_failure(e.message, e.backtrace)
              rescue Test::Unit::AssertionFailedError => e
                add_failure(e.message, e.backtrace)
              rescue Exception
                raise if Test::Unit::TestCase::PASSTHROUGH_EXCEPTIONS.include? $!.class
                add_error($!)
              ensure
                begin
                  teardown
                rescue Mocha::ExpectationError => e
                  add_failure(e.message, e.backtrace)
                rescue Test::Unit::AssertionFailedError => e
                  add_failure(e.message, e.backtrace)
                rescue Exception
                  raise if Test::Unit::TestCase::PASSTHROUGH_EXCEPTIONS.include? $!.class
                  add_error($!)
                end
              end
            ensure
              mocha_teardown
            end
            result.add_run
            yield(Test::Unit::TestCase::FINISHED, name)
          end
        end
      end
    end
  end
end
