require 'concurrent/executor/executor_service'
require 'concurrent/concern/deprecation'

module Concurrent

  # @!visibility private
  module Executor
    extend Concern::Deprecation

    # Get the requested `Executor` based on the values set in the options hash.
    #
    # @param [Hash] opts the options defining the requested executor
    # @option opts [Executor] :executor when set use the given `Executor` instance.
    #   Three special values are also supported: `:fast` returns the global fast executor,
    #   `:io` returns the global io executor, and `:immediate` returns a new
    #   `ImmediateExecutor` object.
    #
    # @return [Executor, nil] the requested thread pool, or nil when no option specified
    #
    # @!visibility private
    def self.executor_from_options(opts = {}) # :nodoc:
      case
      when opts.key?(:executor)
        if opts[:executor].nil?
          nil
        else
          executor(opts[:executor])
        end
      when opts.key?(:operation) || opts.key?(:task)
        if opts[:operation] == true || opts[:task] == false
          deprecated 'use `executor: :fast` instead'
          return Concurrent.global_fast_executor
        end

        if opts[:operation] == false || opts[:task] == true
          deprecated 'use `executor: :io` instead'
          return Concurrent.global_io_executor
        end

        raise ArgumentError.new("executor '#{opts[:executor]}' not recognized")
      else
        nil
      end
    end

    def self.executor(executor_identifier)
      case executor_identifier
      when :fast
        Concurrent.global_fast_executor
      when :io
        Concurrent.global_io_executor
      when :immediate
        Concurrent.global_immediate_executor
      when :operation
        deprecated 'use `executor: :fast` instead'
        Concurrent.global_fast_executor
      when :task
        deprecated 'use `executor: :io` instead'
        Concurrent.global_io_executor
      when Concurrent::ExecutorService
        executor_identifier
      else
        raise ArgumentError, "executor not recognized by '#{executor_identifier}'"
      end
    end
  end
end
