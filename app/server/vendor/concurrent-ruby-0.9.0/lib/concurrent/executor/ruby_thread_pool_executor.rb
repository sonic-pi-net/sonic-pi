require 'thread'

require 'concurrent/atomic/event'
require 'concurrent/concern/logging'
require 'concurrent/executor/executor_service'
require 'concurrent/utility/monotonic_time'

module Concurrent

  # @!macro thread_pool_executor
  # @!macro thread_pool_options
  # @!visibility private
  class RubyThreadPoolExecutor < RubyExecutorService

    # @!macro thread_pool_executor_constant_default_max_pool_size
    DEFAULT_MAX_POOL_SIZE      = 2_147_483_647 # java.lang.Integer::MAX_VALUE

    # @!macro thread_pool_executor_constant_default_min_pool_size
    DEFAULT_MIN_POOL_SIZE      = 0

    # @!macro thread_pool_executor_constant_default_max_queue_size
    DEFAULT_MAX_QUEUE_SIZE     = 0

    # @!macro thread_pool_executor_constant_default_thread_timeout
    DEFAULT_THREAD_IDLETIMEOUT = 60

    # @!macro thread_pool_executor_attr_reader_max_length
    attr_reader :max_length

    # @!macro thread_pool_executor_attr_reader_min_length
    attr_reader :min_length

    # @!macro thread_pool_executor_attr_reader_largest_length
    attr_reader :largest_length

    # @!macro thread_pool_executor_attr_reader_scheduled_task_count
    attr_reader :scheduled_task_count

    # @!macro thread_pool_executor_attr_reader_completed_task_count
    attr_reader :completed_task_count

    # @!macro thread_pool_executor_attr_reader_idletime
    attr_reader :idletime

    # @!macro thread_pool_executor_attr_reader_max_queue
    attr_reader :max_queue

    # @!macro thread_pool_executor_method_initialize
    def initialize(opts = {})
      super(opts)
    end

    # @!macro executor_service_method_can_overflow_question
    def can_overflow?
      synchronize { ns_limited_queue? }
    end

    # @!macro thread_pool_executor_attr_reader_length
    def length
      synchronize { @pool.length }
    end

    # @!macro thread_pool_executor_attr_reader_queue_length
    def queue_length
      synchronize { @queue.length }
    end

    # @!macro thread_pool_executor_attr_reader_remaining_capacity
    def remaining_capacity
      synchronize do
        if ns_limited_queue?
          @max_queue - @queue.length
        else
          -1
        end
      end
    end

    # @!visibility private
    def remove_busy_worker(worker)
      synchronize { ns_remove_busy_worker worker }
    end

    # @!visibility private
    def ready_worker(worker)
      synchronize { ns_ready_worker worker }
    end

    # @!visibility private
    def worker_not_old_enough(worker)
      synchronize { ns_worker_not_old_enough worker }
    end

    # @!visibility private
    def worker_died(worker)
      synchronize { ns_worker_died worker }
    end

    protected

    # @!visibility private
    def ns_initialize(opts)
      @min_length      = opts.fetch(:min_threads, DEFAULT_MIN_POOL_SIZE).to_i
      @max_length      = opts.fetch(:max_threads, DEFAULT_MAX_POOL_SIZE).to_i
      @idletime        = opts.fetch(:idletime, DEFAULT_THREAD_IDLETIMEOUT).to_i
      @max_queue       = opts.fetch(:max_queue, DEFAULT_MAX_QUEUE_SIZE).to_i
      @fallback_policy = opts.fetch(:fallback_policy, opts.fetch(:overflow_policy, :abort))
      raise ArgumentError.new("#{@fallback_policy} is not a valid fallback policy") unless FALLBACK_POLICIES.include?(@fallback_policy)
      deprecated ':overflow_policy is deprecated terminology, please use :fallback_policy instead' if opts.has_key?(:overflow_policy)

      raise ArgumentError.new("`max_threads` cannot be less than #{DEFAULT_MIN_POOL_SIZE}") if @max_length < DEFAULT_MIN_POOL_SIZE
      raise ArgumentError.new("`max_threads` cannot be greater than #{DEFAULT_MAX_POOL_SIZE}") if @max_length > DEFAULT_MAX_POOL_SIZE
      raise ArgumentError.new("`min_threads` cannot be less than #{DEFAULT_MIN_POOL_SIZE}") if @min_length < DEFAULT_MIN_POOL_SIZE
      raise ArgumentError.new("`min_threads` cannot be more than `max_threads`") if min_length > max_length

      self.auto_terminate = opts.fetch(:auto_terminate, true)

      @pool                 = [] # all workers
      @ready                = [] # used as a stash (most idle worker is at the start)
      @queue                = [] # used as queue
      # @ready or @queue is empty at all times
      @scheduled_task_count = 0
      @completed_task_count = 0
      @largest_length       = 0

      @gc_interval  = opts.fetch(:gc_interval, @idletime / 2.0).to_i # undocumented
      @next_gc_time = Concurrent.monotonic_time + @gc_interval
    end

    # @!visibility private
    def ns_limited_queue?
      @max_queue != 0
    end

    # @!visibility private
    def ns_execute(*args, &task)
      if ns_assign_worker(*args, &task) || ns_enqueue(*args, &task)
        @scheduled_task_count += 1
      else
        handle_fallback(*args, &task)
      end

      ns_prune_pool if @next_gc_time < Concurrent.monotonic_time
      # raise unless @ready.empty? || @queue.empty? # assert
    end

    alias_method :execute, :ns_execute

    # @!visibility private
    def ns_shutdown_execution
      if @pool.empty?
        # nothing to do
        stopped_event.set
      end
      if @queue.empty?
        # no more tasks will be accepted, just stop all workers
        @pool.each(&:stop)
      end

      # raise unless @ready.empty? || @queue.empty? # assert
    end

    alias_method :shutdown_execution, :ns_shutdown_execution

    # @!visibility private
    def ns_kill_execution
      # TODO log out unprocessed tasks in queue
      # TODO try to shutdown first?
      @pool.each &:kill
      @pool.clear
      @ready.clear
    end

    alias_method :kill_execution, :ns_kill_execution

    # tries to assign task to a worker, tries to get one from @ready or to create new one
    # @return [true, false] if task is assigned to a worker
    #
    # @!visibility private
    def ns_assign_worker(*args, &task)
      # keep growing if the pool is not at the minimum yet
      worker = (@ready.pop if @pool.size >= @min_length) || ns_add_busy_worker
      if worker
        worker << [task, args]
        true
      else
        false
      end
    rescue ThreadError
      # Raised when the operating system refuses to create the new thread
      return false
    end

    # tries to enqueue task
    # @return [true, false] if enqueued
    #
    # @!visibility private
    def ns_enqueue(*args, &task)
      if !ns_limited_queue? || @queue.size < @max_queue
        @queue << [task, args]
        true
      else
        false
      end
    end

    # @!visibility private
    def ns_worker_died(worker)
      ns_remove_busy_worker worker
      replacement_worker = ns_add_busy_worker
      ns_ready_worker replacement_worker, false if replacement_worker
    end

    # creates new worker which has to receive work to do after it's added
    # @return [nil, Worker] nil of max capacity is reached
    #
    # @!visibility private
    def ns_add_busy_worker
      return if @pool.size >= @max_length

      @pool << (worker = Worker.new(self))
      @largest_length = @pool.length if @pool.length > @largest_length
      worker
    end

    # handle ready worker, giving it new job or assigning back to @ready
    #
    # @!visibility private
    def ns_ready_worker(worker, success = true)
      @completed_task_count += 1 if success
      task_and_args         = @queue.shift
      if task_and_args
        worker << task_and_args
      else
        # stop workers when !running?, do not return them to @ready
        if running?
          @ready.push(worker)
        else
          worker.stop
        end
      end
    end

    # returns back worker to @ready which was not idle for enough time
    #
    # @!visibility private
    def ns_worker_not_old_enough(worker)
      # let's put workers coming from idle_test back to the start (as the oldest worker)
      @ready.unshift(worker)
      true
    end

    # removes a worker which is not in not tracked in @ready
    #
    # @!visibility private
    def ns_remove_busy_worker(worker)
      @pool.delete(worker)
      stopped_event.set if @pool.empty? && !running?
      true
    end

    # try oldest worker if it is idle for enough time, it's returned back at the start
    #
    # @!visibility private
    def ns_prune_pool
      return if @pool.size <= @min_length

      last_used = @ready.shift
      last_used << :idle_test if last_used

      @next_gc_time = Concurrent.monotonic_time + @gc_interval
    end

    # @!visibility private
    class Worker
      include Concern::Logging

      def initialize(pool)
        # instance variables accessed only under pool's lock so no need to sync here again
        @queue  = Queue.new
        @pool   = pool
        @thread = create_worker @queue, pool, pool.idletime
      end

      def <<(message)
        @queue << message
      end

      def stop
        @queue << :stop
      end

      def kill
        @thread.kill
      end

      private

      def create_worker(queue, pool, idletime)
        Thread.new(queue, pool, idletime) do |queue, pool, idletime|
          last_message = Concurrent.monotonic_time
          catch(:stop) do
            loop do

              case message = queue.pop
              when :idle_test
                if (Concurrent.monotonic_time - last_message) > idletime
                  pool.remove_busy_worker(self)
                  throw :stop
                else
                  pool.worker_not_old_enough(self)
                end

              when :stop
                pool.remove_busy_worker(self)
                throw :stop

              else
                task, args = message
                run_task pool, task, args
                last_message = Concurrent.monotonic_time

                pool.ready_worker(self)
              end

            end
          end
        end
      end

      def run_task(pool, task, args)
        task.call(*args)
      rescue => ex
        # let it fail
        log DEBUG, ex
      rescue Exception => ex
        log ERROR, ex
        pool.worker_died(self)
        throw :stop
      end
    end

    private_constant :Worker
  end
end
