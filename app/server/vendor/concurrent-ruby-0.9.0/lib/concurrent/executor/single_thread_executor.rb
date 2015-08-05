require 'concurrent/executor/ruby_single_thread_executor'

module Concurrent

  if Concurrent.on_jruby?
    require 'concurrent/executor/java_single_thread_executor'
  end

  SingleThreadExecutorImplementation = case
                                       when Concurrent.on_jruby?
                                         JavaSingleThreadExecutor
                                       else
                                         RubySingleThreadExecutor
                                       end
  private_constant :SingleThreadExecutorImplementation

  # @!macro [attach] single_thread_executor
  #
  #   A thread pool with a set number of threads. The number of threads in the pool
  #   is set on construction and remains constant. When all threads are busy new
  #   tasks `#post` to the thread pool are enqueued until a thread becomes available.
  #   Should a thread crash for any reason the thread will immediately be removed
  #   from the pool and replaced.
  #
  #   The API and behavior of this class are based on Java's `SingleThreadExecutor`
  #
  # @!macro thread_pool_options
  # @!macro abstract_executor_service_public_api
  class SingleThreadExecutor < SingleThreadExecutorImplementation

    # @!macro [new] single_thread_executor_method_initialize
    #
    #   Create a new thread pool.
    #
    #   @option opts [Symbol] :fallback_policy (:discard) the policy for
    #     handling new tasks that are received when the queue size has
    #     reached `max_queue` or after the executor has shut down
    #
    #   @see http://docs.oracle.com/javase/tutorial/essential/concurrency/pools.html
    #   @see http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Executors.html
    #   @see http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html

    # @!method initialize(opts = {})
    #   @!macro single_thread_executor_method_initialize
  end
end
