require 'concurrent/errors'
require 'concurrent/concern/deprecation'
require 'concurrent/concern/logging'
require 'concurrent/atomic/event'
require 'concurrent/synchronization'
require 'concurrent/utility/at_exit'

module Concurrent

  # @!macro [new] executor_service_method_post
  #
  #   Submit a task to the executor for asynchronous processing.
  #
  #   @param [Array] args zero or more arguments to be passed to the task
  #
  #   @yield the asynchronous task to perform
  #
  #   @return [Boolean] `true` if the task is queued, `false` if the executor
  #     is not running
  #
  #   @raise [ArgumentError] if no task is given

  # @!macro [new] executor_service_method_left_shift
  #
  #   Submit a task to the executor for asynchronous processing.
  #
  #   @param [Proc] task the asynchronous task to perform
  #
  #   @return [self] returns itself

  # @!macro [new] executor_service_method_can_overflow_question
  #
  #   Does the task queue have a maximum size?
  #
  #   @return [Boolean] True if the task queue has a maximum size else false.

  # @!macro [new] executor_service_method_serialized_question
  #
  #   Does this executor guarantee serialization of its operations?
  #
  #   @return [Boolean] True if the executor guarantees that all operations
  #     will be post in the order they are received and no two operations may
  #     occur simultaneously. Else false.





  # @!macro [new] executor_service_public_api
  #
  #   @!method post(*args, &task)
  #     @!macro executor_service_method_post
  #
  #   @!method <<(task)
  #     @!macro executor_service_method_left_shift
  #
  #   @!method can_overflow?
  #     @!macro executor_service_method_can_overflow_question
  #
  #   @!method serialized?
  #     @!macro executor_service_method_serialized_question





  # @!macro executor_service_public_api
  # @!visibility private
  module ExecutorService
    include Concern::Logging
    include Concern::Deprecation

    # @!macro executor_service_method_post
    def post(*args, &task)
      raise NotImplementedError
    end

    # @!macro executor_service_method_left_shift
    def <<(task)
      post(&task)
      self
    end

    # @!macro executor_service_method_can_overflow_question
    #
    # @note Always returns `false`
    def can_overflow?
      false
    end

    # @!macro executor_service_method_serialized_question
    #
    # @note Always returns `false`
    def serialized?
      false
    end
  end

  # Indicates that the including `ExecutorService` guarantees
  # that all operations will occur in the order they are post and that no
  # two operations may occur simultaneously. This module provides no
  # functionality and provides no guarantees. That is the responsibility
  # of the including class. This module exists solely to allow the including
  # object to be interrogated for its serialization status.
  #
  # @example
  #   class Foo
  #     include Concurrent::SerialExecutor
  #   end
  #
  #   foo = Foo.new
  #
  #   foo.is_a? Concurrent::ExecutorService #=> true
  #   foo.is_a? Concurrent::SerialExecutor  #=> true
  #   foo.serialized?                       #=> true
  #
  # @!visibility private
  module SerialExecutorService
    include ExecutorService

    # @!macro executor_service_method_serialized_question
    #
    # @note Always returns `true`
    def serialized?
      true
    end
  end





  # @!macro [new] executor_service_attr_reader_fallback_policy
  #   @return [Symbol] The fallback policy in effect. Either `:abort`, `:discard`, or `:caller_runs`.

  # @!macro [new] executor_service_method_shutdown
  #
  #   Begin an orderly shutdown. Tasks already in the queue will be executed,
  #   but no new tasks will be accepted. Has no additional effect if the
  #   thread pool is not running.

  # @!macro [new] executor_service_method_kill
  #
  #   Begin an immediate shutdown. In-progress tasks will be allowed to
  #   complete but enqueued tasks will be dismissed and no new tasks
  #   will be accepted. Has no additional effect if the thread pool is
  #   not running.

  # @!macro [new] executor_service_method_wait_for_termination
  #
  #   Block until executor shutdown is complete or until `timeout` seconds have
  #   passed.
  #
  #   @note Does not initiate shutdown or termination. Either `shutdown` or `kill`
  #     must be called before this method (or on another thread).
  #
  #   @param [Integer] timeout the maximum number of seconds to wait for shutdown to complete
  #
  #   @return [Boolean] `true` if shutdown complete or false on `timeout`

  # @!macro [new] executor_service_method_running_question
  #
  #   Is the executor running?
  #
  #   @return [Boolean] `true` when running, `false` when shutting down or shutdown

  # @!macro [new] executor_service_method_shuttingdown_question
  #
  #   Is the executor shuttingdown?
  #
  #   @return [Boolean] `true` when not running and not shutdown, else `false`

  # @!macro [new] executor_service_method_shutdown_question
  #
  #   Is the executor shutdown?
  #
  #   @return [Boolean] `true` when shutdown, `false` when shutting down or running

  # @!macro [new] executor_service_method_auto_terminate_question
  #
  #   Is the executor auto-terminate when the application exits?
  #
  #   @return [Boolean] `true` when auto-termination is enabled else `false`.

  # @!macro [new] executor_service_method_auto_terminate_setter
  #
  #   Set the auto-terminate behavior for this executor.
  #
  #   @param [Boolean] value The new auto-terminate value to set for this executor.
  #
  #   @return [Boolean] `true` when auto-termination is enabled else `false`.





  # @!macro [new] abstract_executor_service_public_api
  #
  #   @!macro executor_service_public_api
  #
  #   @!attribute [r] fallback_policy
  #     @!macro executor_service_attr_reader_fallback_policy
  #
  #   @!method shutdown
  #     @!macro executor_service_method_shutdown
  #
  #   @!method kill
  #     @!macro executor_service_method_kill
  #
  #   @!method wait_for_termination(timeout = nil)
  #     @!macro executor_service_method_wait_for_termination
  #
  #   @!method running?
  #     @!macro executor_service_method_running_question
  #
  #   @!method shuttingdown?
  #     @!macro executor_service_method_shuttingdown_question
  #
  #   @!method shutdown?
  #     @!macro executor_service_method_shutdown_question
  #
  #   @!method auto_terminate?
  #     @!macro executor_service_method_auto_terminate_question
  #
  #   @!method auto_terminate=(value)
  #     @!macro executor_service_method_auto_terminate_setter





  # @!macro abstract_executor_service_public_api
  # @!visibility private
  class AbstractExecutorService < Synchronization::Object
    include ExecutorService

    # The set of possible fallback policies that may be set at thread pool creation.
    FALLBACK_POLICIES = [:abort, :discard, :caller_runs].freeze

    # @!macro [attach] executor_service_attr_reader_fallback_policy
    attr_reader :fallback_policy

    # Create a new thread pool.
    def initialize(*args, &block)
      super(&nil)
      synchronize { ns_initialize(*args, &block) }
    end

    # @!macro [attach] executor_service_method_shutdown
    def shutdown
      raise NotImplementedError
    end

    # @!macro [attach] executor_service_method_kill
    def kill
      raise NotImplementedError
    end

    # @!macro [attach] executor_service_method_wait_for_termination
    def wait_for_termination(timeout = nil)
      raise NotImplementedError
    end

    # @!macro [attach] executor_service_method_running_question
    def running?
      synchronize { ns_running? }
    end

    # @!macro [attach] executor_service_method_shuttingdown_question
    def shuttingdown?
      synchronize { ns_shuttingdown? }
    end

    # @!macro [attach] executor_service_method_shutdown_question
    def shutdown?
      synchronize { ns_shutdown? }
    end

    # @!macro [attach] executor_service_method_auto_terminate_question
    def auto_terminate?
      synchronize { ns_auto_terminate? }
    end

    # @!macro [attach] executor_service_method_auto_terminate_setter
    def auto_terminate=(value)
      synchronize { self.ns_auto_terminate = value }
    end

    protected

    # Handler which executes the `fallback_policy` once the queue size
    # reaches `max_queue`.
    #
    # @param [Array] args the arguments to the task which is being handled.
    #
    # @!visibility private
    def handle_fallback(*args)
      case fallback_policy
      when :abort
        raise RejectedExecutionError
      when :discard
        false
      when :caller_runs
        begin
          yield(*args)
        rescue => ex
          # let it fail
          log DEBUG, ex
        end
        true
      else
        fail "Unknown fallback policy #{fallback_policy}"
      end
    end

    def execute(*args, &task)
      raise NotImplementedError
    end

    # @!macro [attach] executor_service_method_shutdown_execution
    #
    #   Callback method called when an orderly shutdown has completed.
    #   The default behavior is to signal all waiting threads.
    def shutdown_execution
      # do nothing
    end

    # @!macro [attach] executor_service_method_kill_execution
    #
    #   Callback method called when the executor has been killed.
    #   The default behavior is to do nothing.
    def kill_execution
      # do nothing
    end

    protected

    def ns_auto_terminate?
      !!@auto_terminate
    end

    def ns_auto_terminate=(value)
      case value
      when true
        AtExit.add(self) { terminate_at_exit }
        @auto_terminate = true
      when false
        AtExit.delete(self)
        @auto_terminate = false
      else
        raise ArgumentError
      end
    end

    def terminate_at_exit
      kill # TODO be gentle first
      wait_for_termination(10)
    end
  end

  # @!macro abstract_executor_service_public_api
  # @!visibility private
  class RubyExecutorService < AbstractExecutorService

    def initialize(*args, &block)
      super
      @stop_event    = Event.new
      @stopped_event = Event.new
      ensure_ivar_visibility!
    end

    def post(*args, &task)
      raise ArgumentError.new('no block given') unless block_given?
      synchronize do
        # If the executor is shut down, reject this task
        return handle_fallback(*args, &task) unless running?
        execute(*args, &task)
        true
      end
    end

    def shutdown
      synchronize do
        break unless running?
        self.ns_auto_terminate = false
        stop_event.set
        shutdown_execution
      end
      true
    end

    def kill
      synchronize do
        break if shutdown?
        self.ns_auto_terminate = false
        stop_event.set
        kill_execution
        stopped_event.set
      end
      true
    end

    def wait_for_termination(timeout = nil)
      stopped_event.wait(timeout)
    end

    protected

    attr_reader :stop_event, :stopped_event

    def shutdown_execution
      stopped_event.set
    end

    def ns_running?
      !stop_event.set?
    end

    def ns_shuttingdown?
      !(ns_running? || ns_shutdown?)
    end

    def ns_shutdown?
      stopped_event.set?
    end
  end

  if Concurrent.on_jruby?

    # @!macro abstract_executor_service_public_api
    # @!visibility private
    class JavaExecutorService < AbstractExecutorService
      java_import 'java.lang.Runnable'

      FALLBACK_POLICY_CLASSES = {
        abort:       java.util.concurrent.ThreadPoolExecutor::AbortPolicy,
        discard:     java.util.concurrent.ThreadPoolExecutor::DiscardPolicy,
        caller_runs: java.util.concurrent.ThreadPoolExecutor::CallerRunsPolicy
      }.freeze
      private_constant :FALLBACK_POLICY_CLASSES

      def initialize(*args, &block)
        super
        ns_make_executor_runnable
      end

      def post(*args, &task)
        raise ArgumentError.new('no block given') unless block_given?
        return handle_fallback(*args, &task) unless running?
        @executor.submit_runnable Job.new(args, task)
        true
      rescue Java::JavaUtilConcurrent::RejectedExecutionException
        raise RejectedExecutionError
      end

      def wait_for_termination(timeout = nil)
        if timeout.nil?
          ok = @executor.awaitTermination(60, java.util.concurrent.TimeUnit::SECONDS) until ok
          true
        else
          @executor.awaitTermination(1000 * timeout, java.util.concurrent.TimeUnit::MILLISECONDS)
        end
      end

      def shutdown
        synchronize do
          self.ns_auto_terminate = false
          @executor.shutdown
          nil
        end
      end

      def kill
        synchronize do
          self.ns_auto_terminate = false
          @executor.shutdownNow
          nil
        end
      end

      protected

      def ns_running?
        !(ns_shuttingdown? || ns_shutdown?)
      end

      def ns_shuttingdown?
        if @executor.respond_to? :isTerminating
          @executor.isTerminating
        else
          false
        end
      end

      def ns_shutdown?
        @executor.isShutdown || @executor.isTerminated
      end

      def ns_make_executor_runnable
        if !defined?(@executor.submit_runnable)
          @executor.class.class_eval do
            java_alias :submit_runnable, :submit, [java.lang.Runnable.java_class]
          end
        end
      end

      class Job
        include Runnable
        def initialize(args, block)
          @args = args
          @block = block
        end

        def run
          @block.call(*@args)
        end
      end
      private_constant :Job
    end
  end
end
