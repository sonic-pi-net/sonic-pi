require 'concurrent/executor/executor_service'

module Concurrent

  # @!macro single_thread_executor
  # @!macro thread_pool_options
  # @!macro abstract_executor_service_public_api
  # @!visibility private
  class RubySingleThreadExecutor < RubyExecutorService
    include SerialExecutorService

    # @!macro single_thread_executor_method_initialize
    def initialize(opts = {})
      super
    end

    protected

    def ns_initialize(opts)
      @queue = Queue.new
      @thread = nil
      @fallback_policy = opts.fetch(:fallback_policy, :discard)
      raise ArgumentError.new("#{@fallback_policy} is not a valid fallback policy") unless FALLBACK_POLICIES.include?(@fallback_policy)
      self.auto_terminate = opts.fetch(:auto_terminate, true)
    end

    # @!visibility private
    def execute(*args, &task)
      supervise
      @queue << [args, task]
    end

    # @!visibility private
    def shutdown_execution
      @queue << :stop
      stopped_event.set unless alive?
    end

    # @!visibility private
    def kill_execution
      @queue.clear
      @thread.kill if alive?
    end

    # @!visibility private
    def alive?
      @thread && @thread.alive?
    end

    # @!visibility private
    def supervise
      @thread = new_worker_thread unless alive?
    end

    # @!visibility private
    def new_worker_thread
      Thread.new do
        Thread.current.abort_on_exception = false
        work
      end
    end

    # @!visibility private
    def work
      loop do
        task = @queue.pop
        break if task == :stop
        begin
          task.last.call(*task.first)
        rescue => ex
          # let it fail
          log DEBUG, ex
        end
      end
      stopped_event.set
    end
  end
end
