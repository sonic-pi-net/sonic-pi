require 'rbconfig'
require 'concurrent/utility/native_extension_loader'

module Concurrent
  module TestHelpers
    extend self

    def delta(v1, v2)
      if block_given?
        v1 = yield(v1)
        v2 = yield(v2)
      end
      return (v1 - v2).abs
    end

    include Utility::EngineDetector

    def use_c_extensions?
      Concurrent.allow_c_extensions?
    end

    GLOBAL_EXECUTORS = [
      [:GLOBAL_FAST_EXECUTOR, -> { Delay.new { Concurrent.new_fast_executor(auto_terminate: true) } }],
      [:GLOBAL_IO_EXECUTOR, -> { Delay.new { Concurrent.new_io_executor(auto_terminate: true) } }],
      [:GLOBAL_TIMER_SET, -> { Delay.new { Concurrent::TimerSet.new(auto_terminate: true) } }],
    ]

    def reset_gem_configuration
      GLOBAL_EXECUTORS.each do |var, factory|
        executor = Concurrent.const_get(var).value!
        executor.shutdown
        executor.wait_for_termination(0.2)
        executor.kill
        Concurrent.const_set(var, factory.call)
      end
    end

    def monotonic_interval
      raise ArgumentError.new('no block given') unless block_given?
      start_time = GLOBAL_MONOTONIC_CLOCK.get_time
      yield
      GLOBAL_MONOTONIC_CLOCK.get_time - start_time
    end
  end
end

class RSpec::Core::ExampleGroup
  include Concurrent::TestHelpers
  extend Concurrent::TestHelpers
end
