
## Examples

**Limiting concurrency level of a concurrently executed block to two** 

```ruby
max_two = Concurrent::Throttle.new 2

# used to track concurrency level
concurrency_level = Concurrent::AtomicFixnum.new
job = -> do
  # increase the current level at the beginning of the throttled block    
  concurrency_level.increment
  # work, takes some time
  do_stuff
  # read the current concurrency level 
  current_concurrency_level = concurrency_level.value
  # decrement the concurrency level back at the end of the block            
  concurrency_level.decrement
  # return the observed concurrency level 
  current_concurrency_level
end #

# create 10 threads running concurrently the jobs
Array.new(10) do  
  Thread.new do
    max_two.acquire(&job)   
  end
# wait for all the threads to finish and read the observed 
# concurrency level in each of them   
end.map(&:value)                         # => [2, 2, 1, 1, 1, 2, 2, 2, 2, 1]
```
Notice that the returned array has no number bigger than 2 therefore 
the concurrency level of the block with the `do_stuff` was never bigger than 2. 

```ruby
# runs a block, and returns he observed concurrency level during the execution
def monitor_concurrency_level(concurrency_level, &block)
  concurrency_level.increment
  block.call
  current_concurrency_level = concurrency_level.value
  concurrency_level.decrement
  # return the observed concurrency level
  return current_concurrency_level 
end #

throttle = Concurrent::Throttle.new 3
concurrency_level = Concurrent::AtomicFixnum.new 

Array.new(10) do |i|
  # create throttled future
  throttle.future(i) do |arg|
    monitor_concurrency_level(concurrency_level) { do_stuff arg }  
    # fulfill with the observed concurrency level
  end
# collect observed concurrency levels   
end.map(&:value!)                        # => [3, 2, 1, 2, 1, 3, 3, 1, 2, 1]
```
The concurrency level does not rise above 3.

It works by setting the executor of the future created from the throttle. 
The executor is a proxy executor for the `Concurrent::Promises.default_executor` 
which can be obtained using {Concurrent::Throttle#on} method. 
Therefore the above example could be instead more explicitly written as follows
 
```ruby
# ...
Array.new(10) do |i|
  # create throttled future
  Concurrent::Promises.future_on(throttle.on(Concurrent::Promises.default_executor)) do
    # ...
  end
end.map(&:value!) #
```

Anything executed on the proxy executor is throttled. 
A throttle can have more proxy executors for different executors, 
all jobs share the same capacity provided by the throttle.

Since the proxy executor becomes the executor of the future, 
any chained futures will also be throttled. 
It can be changed by using different executor.
It the following example the first 2 futures in the chain are throttled, 
the last is not.

```ruby
concurrency_level_throttled   = Concurrent::AtomicFixnum.new #
concurrency_level_unthrottled = Concurrent::AtomicFixnum.new #
Array.new(10) do |i|
  throttle.future(i) do 
    monitor_concurrency_level(concurrency_level_throttled) { do_stuff } 
  end.then do |v|
    [v, monitor_concurrency_level(concurrency_level_throttled) { do_stuff }]
  end.then_on(:io) do |l1, l2|
    [l1, l2, monitor_concurrency_level(concurrency_level_unthrottled) { 5.times { do_stuff } }]
  end
end.map(&:value!) #
# => [[3, 3, 7],
#     [3, 2, 9],
#     [3, 3, 10],
#     [3, 3, 6],
#     [3, 3, 5],
#     [3, 3, 8],
#     [3, 3, 3],
#     [3, 3, 4],
#     [3, 2, 2],
#     [3, 1, 1]]
```

In the output you can see that the first 2 columns do not cross the 3 capacity limit
and the last column which is untroubled does.

TODO (pitr-ch 20-Dec-2018): example with virtual throttled executor, 
throttling only part of promises chain.  

**Other abstraction**

The proxy executor created with throttle can be used with other abstractions as well 
and combined.

```ruby
concurrency_level = Concurrent::AtomicFixnum.new #
futures = Array.new(5) do |i|
  # create throttled future
  throttle.future(i) do |arg|
    monitor_concurrency_level(concurrency_level) { do_stuff arg }  
    # fulfill with the observed concurrency level
  end
end #
agents = Array.new(5) do |i|
  agent = Concurrent::Agent.new 0
  # execute agent update on throttled executor
  agent.send_via(throttle.on(:io)) { monitor_concurrency_level(concurrency_level_throttled) { do_stuff } }
  agent 
end #
futures.map(&:value!)                    # => [3, 3, 3, 2, 1]
agents.each { |a| a.await }.map(&:value) #
# => [3, 2, 3, 3, 1]
```

There is no observed concurrency level above 3.
