# Thread Pools

A Thread Pool is an abstraction that you can give a unit of work to, and the work will be executed by one of possibly several threads in the pool. One motivation for using thread pools is the overhead of creating and destroying threads. Creating a pool of reusable worker threads then repeatedly re-using threads from the pool can have huge performance benefits for a long-running application like a service.

`concurrent-ruby` also offers some higher level abstractions than thread pools. For many problems, you will be better served by using one of these -- if you are thinking of using a thread pool, we especially recommend you look at and understand [Future](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/Future.html)s before deciding to use thread pools directly instead.  Futures are implemented using thread pools, but offer a higher level abstraction.

But there are some problems for which directly using a thread pool is an appropriate solution. Or, you may wish to make your own thread pool to run Futures on, to be separate or have different characteristics than the global thread pool that Futures run on by default.

Thread pools are considered 'executors' -- an object you can give a unit of work to, to have it executed.  In fact, thread pools are the main kind of executor you will see - others are mainly for testing or odd edge cases. In some documentation or source code you'll see reference to an 'executor' -- this is commonly a thread pool, or else something similar that executes units of work (usually supplied as Ruby blocks).

## FixedThreadPool

A [FixedThreadPool](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/FixedThreadPool.html) contains a fixed number of threads. When you give a unit of work to it, an available thread will be used to execute.

~~~ruby
pool = Concurrent::FixedThreadPool.new(5) # 5 threads
pool.post do
   # some parallel work
end
# As with all thread pools, execution resumes immediately here in the caller thread,
# while work is concurrently being done in the thread pool, at some possibly future point.
~~~

What happens if you post new work when all (e.g.) 5 threads are currently busy? It will be added to a queue, and executed when a thread becomes available.  In a `FixedThreadPool`, if you post work to the pool much faster than the work can be completed, the queue may grow without bounds, as the work piles up in the holding queue, using up memory without bounds.  To limit the queue and apply some form of 'back pressure' instead, you can use the more configurable `ThreadPoolExecutor` (See below).

If you'd like to base the number of threads in the pool on the number of processors available, your code can consult [Concurrent.processor_count](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/ProcessorCounter.html#processor_count-instance_method).

The `FixedThreadPool` is based on the semantics used in Java for [java.util.concurrent.Executors.newFixedThreadPool(int nThreads)](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Executors.html#newFixedThreadPool(int))

## CachedThreadPool

A [CachedThreadPool](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/CachedThreadPool.html) will create as many threads as necessary for work posted to it. If you post work to a `CachedThreadPool` when all its existing threads are busy, it will create a new thread to execute that work, and then keep that thread cached for future work. Cached threads are reclaimed (destroyed) after they are idle for a while.

CachedThreadPools typically improve the performance of programs that execute many short-lived asynchronous tasks.

~~~ruby
pool = Concurrent::CachedThreadPool.new
pool.post do
  # some parallel work
end
~~~

The behavior of `CachedThreadPool` is based on Java's [java.util.concurrent.Executors.newCachedThreadPool()](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Executors.html#newCachedThreadPool())

If you'd like to configure a maximum number of threads, you can use the more general configurable `ThreadPoolExecutor`.

## ThreadPoolExecutor

A [ThreadPoolExecutor](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/ThreadPoolExecutor.html) is a general-purpose thread pool that can be configured to have various behaviors.

The `CachedThreadPool` and `FixedThreadPool` are simply `ThreadPoolExecutor`s with certain configuration pre-determined. For instance, to create a `ThreadPoolExecutor` that works just like a `FixedThreadPool.new 5`, you could:

~~~ruby
pool = Concurrent::ThreadPoolExecutor.new(
   min_threads: 5,
   max_threads: 5,
   max_queue: 0 # unbounded work queue
)
~~~

If you want to provide a maximum queue size, you may also consider the `fallback_policy` -- what will happen if work is posted to a pool when the queue of waiting work has reached the maximum size? Available policies:

* abort: Raise a `Concurrent::RejectedExecutionError` exception and discard the task. (default policy)
* discard: Silently discard the task and return nil as the task result.
* caller_runs: The work will be executed in the thread of the caller, instead of being given to another thread in the pool.

~~~ruby
pool = Concurrent::ThreadPoolExecutor.new(
   min_threads: 5,
   max_threads: 5,
   max_queue: 100,
   fallback_policy: :caller_runs
)
~~~

Similarly, you can create something similar to a `CachedThreadPool`, but with a maximum number of threads. With an unbounded queue:

~~~ruby
pool = Concurrent::ThreadPoolExecutor.new(
   min_threads: 3, # create 3 threads at startup
   max_threads: 10, # create at most 10 threads
   max_queue: 0, # unbounded queue of work waiting for an available thread
)
~~~

Or, with a variable number of threads like a CachedThreadPool, but with a bounded queue and a fallback_policy:

~~~ruby
pool = Concurrent::ThreadPoolExecutor.new(
   min_threads: 3, # create 3 threads at startup
   max_threads: 10, # create at most 10 threads
   max_queue: 100, # at most 100 jobs waiting in the queue,
   fallback_policy: :abort
)
~~~

ThreadPoolExecutors with `min_threads` and `max_threads` set to different values will ordinarily reclaim idle threads.  You can supply an `idletime` argument, number of seconds that a thread may be idle before being reclaimed. The default is 60 seconds.

`concurrent-ruby` thread pools are based on designs from `java.util.concurrent` --  a well-designed, stable, scalable, and battle-tested concurrency library. The `ThreadPoolExecutor` is based on Java [java.util.concurrent.ThreadPoolExecutor](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ThreadPoolExecutor.html), and is in fact implemented with a Java ThreadPoolExecutor when running under JRuby. For more information on the design and concepts, you may find the Java documentation helpful:

* http://docs.oracle.com/javase/tutorial/essential/concurrency/pools.html
* http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Executors.html
* http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html

## Thread Pool Status and Shutdown

A running thread pool can be shutdown in an orderly or disruptive manner. Once a thread pool has been shutdown it cannot be started again.

The `shutdown` method can be used to initiate an orderly shutdown of the thread pool. All new post calls will be handled according to the `fallback_policy` (i.e. failing with a RejectedExecutionError by default). Threads in the pool will continue to process all in-progress work and will process all tasks still in the queue.

The `kill` method can be used to immediately shutdown the pool. All new post calls will be handled according to the `fallback_policy`. Ruby's `Thread.kill` will be called on all threads in the pool, aborting all in-progress work. Tasks in the queue will be discarded.

The method `wait_for_termination` can be used to block and wait for pool shutdown to complete. This is useful when shutting down an application and ensuring the app doesn't exit before pool processing is complete. The method wait_for_termination will block for a maximum of the given number of seconds then return true (if shutdown completed successfully) or false (if it was still ongoing). When the timeout value is `nil` the call will block indefinitely. Calling `wait_for_termination` on a stopped thread pool will immediately return true.

~~~ruby
# tell the pool to shutdown in an orderly fashion, allowing in progress work to complete
pool.shutdown
# now wait for all work to complete, wait as long as it takes
pool.wait_for_termination
~~~

You can check for current pool status:

~~~ruby
pool.running?
pool.shuttingdown? # in process of shutting down, can't take any more work
pool.shutdown? # it's done
~~~

The `shutdown?` method will return true for a stopped pool, regardless of whether the pool was stopped with `shutdown` or `kill`.

## Other Executors

  There are several other thread pools and executors in the `concurrent-ruby` library. See the API documentation for more information:

  * [CachedThreadPool](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/CachedThreadPool.html)
  * [FixedThreadPool](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/FixedThreadPool.html)
  * [ImmediateExecutor](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/ImmediateExecutor.html)
  * [PerThreadExecutor](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/PerThreadExecutor.html)
  * [SafeTaskExecutor](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/SafeTaskExecutor.html)
  * [SerializedExecution](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/SerializedExecution.html)
  * [SerializedExecutionDelegator](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/SerializedExecutionDelegator.html)
  * [SingleThreadExecutor](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/SingleThreadExecutor.html)
  * [ThreadPoolExecutor](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/ThreadPoolExecutor.html)
  * [TimerSet](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/TimerSet.html)

## Global Thread Pools

Concurrent Ruby provides several global thread pools. Higher-level abstractions use global thread pools, by default, for running asynchronous operations without creating new threads more often than necessary. These executors are lazy-loaded so they do not create overhead when not needed. The global executors may also be accessed directly if desired. For more information regarding the global thread pools and their configuration, refer to the [API documentation](http://ruby-concurrency.github.io/concurrent-ruby/Concurrent/Configuration.html).

When using a higher-level abstraction, which ordinarily uses a global thread pool, you may wish to instead supply your own thread pool, for separation of work, or to control the thread pool behavior with configuration.

~~~ruby
pool = Concurrent::ThreadPoolExecutor.new(
  :min_threads => [2, Concurrent.processor_count].max,
  :max_threads => [2, Concurrent.processor_count].max,
  :max_queue   => [2, Concurrent.processor_count].max * 5,
  :fallback_policy => :caller_runs
)

future = Future.new(:executor => pool).execute do
   #work
end
~~~
