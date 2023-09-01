
## Examples

**Run async task until cancelled**

Create cancellation and then run work in a background thread until it is cancelled. 

```ruby
cancellation, origin = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x000002 pending>
# - origin is used for cancelling, resolve it to cancel 
# - cancellation is passed down to tasks for cooperative cancellation
async_task = Concurrent::Promises.future(cancellation) do |cancellation|
  # Do work repeatedly until it is cancelled
  do_stuff until cancellation.canceled?
  :stopped_gracefully
end
# => #<Concurrent::Promises::Future:0x000003 pending>

sleep 0.01                               # => 0
# Wait a bit then stop the thread by resolving the origin of the cancellation
origin.resolve 
# => #<Concurrent::Promises::ResolvableEvent:0x000004 resolved>
async_task.value!                        # => :stopped_gracefully
```

Or let it raise an error.

```ruby
cancellation, origin = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x000005 pending>
async_task = Concurrent::Promises.future(cancellation) do |cancellation|
  # Do work repeatedly until it is cancelled
  while true
    cancellation.check!     
    do_stuff 
  end
end
# => #<Concurrent::Promises::Future:0x000006 pending>

sleep 0.01                               # => 0
# Wait a bit then stop the thread by resolving the origin of the cancellation
origin.resolve 
# => #<Concurrent::Promises::ResolvableEvent:0x000007 resolved>
async_task.result
# => [false,
#     nil,
#     #<Concurrent::CancelledOperationError: Concurrent::CancelledOperationError>]
```

**Run additional tasks on Cancellation** 

Cancellation can also be used to log or plan re-execution.
  
```ruby
cancellation.origin.chain do
  # This block is executed after the Cancellation is cancelled  
  # It can then log cancellation or e.g. plan new re-execution
end
# => #<Concurrent::Promises::Future:0x000008 pending>
```

**Run only for limited time – Timeout replacement**

Execute task for a given time then finish. 
Instead of letting Cancellation crate its own origin, it can be passed in as argument.
The passed in origin is scheduled to be resolved in given time which then cancels the Cancellation.

```ruby
timeout = Concurrent::Cancellation.new Concurrent::Promises.schedule(0.02)
# => #<Concurrent::Cancellation:0x000009 pending>
# or using shortcut helper method
timeout = Concurrent::Cancellation.timeout 0.02 
# => #<Concurrent::Cancellation:0x00000a pending>
count   = Concurrent::AtomicFixnum.new
# => #<Concurrent::AtomicFixnum:0x00000b value:0>
Concurrent.global_io_executor.post(timeout) do |timeout|
  # do stuff until cancelled  
  count.increment until timeout.canceled?
end 

timeout.origin.wait
# => #<Concurrent::Promises::Event:0x00000c resolved>
count.value                              # => 177576
```

**Parallel background processing with single cancellation**

Each task tries to count to 1000 but there is a randomly failing test. The
tasks share single cancellation, when one of them fails it cancels the others.
The failing tasks ends with an error, the other tasks are gracefully cancelled.

```ruby
cancellation, origin = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x00000d pending>
tasks = 4.times.map do |i|
  Concurrent::Promises.future(cancellation, origin, i) do |cancellation, origin, i|
    count = 0
    100.times do
      break count = :cancelled if cancellation.canceled?
      count += 1
      sleep 0.001
      if rand > 0.95
        origin.resolve # cancel
        raise 'random error'
      end
      count
    end
  end
end
# => [#<Concurrent::Promises::Future:0x00000e pending>,
#     #<Concurrent::Promises::Future:0x00000f pending>,
#     #<Concurrent::Promises::Future:0x000010 pending>,
#     #<Concurrent::Promises::Future:0x000011 pending>]
Concurrent::Promises.zip(*tasks).result 
# => [false,
#     [:cancelled, nil, :cancelled, :cancelled],
#     [nil, #<RuntimeError: random error>, nil, nil]]
```

Without the randomly failing part it produces following.

```ruby
cancellation, origin = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x000012 pending>
tasks = 4.times.map do |i|
  Concurrent::Promises.future(cancellation, origin, i) do |cancellation, origin, i|
    count = 0
    100.times do
      break count = :cancelled if cancellation.canceled?
      count += 1
      sleep 0.001
      # if rand > 0.95
      #   origin.resolve
      #   raise 'random error'
      # end
      count
    end
  end
end
# => [#<Concurrent::Promises::Future:0x000013 pending>,
#     #<Concurrent::Promises::Future:0x000014 pending>,
#     #<Concurrent::Promises::Future:0x000015 pending>,
#     #<Concurrent::Promises::Future:0x000016 pending>]
Concurrent::Promises.zip(*tasks).result
# => [true, [100, 100, 100, 100], nil]
```

**Combine cancellations**

The combination created by joining two cancellations cancels when the first or the other does.

```ruby
cancellation_a, origin_a = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x000017 pending>
cancellation_b, origin_b = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x000018 pending>
combined_cancellation    = cancellation_a.join(cancellation_b)
# => #<Concurrent::Cancellation:0x000019 pending>

origin_a.resolve
# => #<Concurrent::Promises::ResolvableEvent:0x00001a resolved>

cancellation_a.canceled?                 # => true
cancellation_b.canceled?                 # => false
combined_cancellation.canceled?          # => true
```

If a different rule for joining is needed, the source can be combined manually.
The manually created cancellation cancels when both the first and the other cancels.

```ruby
cancellation_a, origin_a = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x00001b pending>
cancellation_b, origin_b = Concurrent::Cancellation.new
# => #<Concurrent::Cancellation:0x00001c pending>
# cancels only when both a and b is cancelled
combined_cancellation    = Concurrent::Cancellation.new origin_a & origin_b
# => #<Concurrent::Cancellation:0x00001d pending>

origin_a.resolve
# => #<Concurrent::Promises::ResolvableEvent:0x00001e resolved>

cancellation_a.canceled?        #=> true
cancellation_b.canceled?        #=> false
combined_cancellation.canceled? #=> false

origin_b.resolve
# => #<Concurrent::Promises::ResolvableEvent:0x00001f resolved>
combined_cancellation.canceled? #=> true
```

