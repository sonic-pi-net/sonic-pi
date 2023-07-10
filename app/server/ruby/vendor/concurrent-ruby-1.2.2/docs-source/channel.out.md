## Examples

Let's start by creating a channel with a capacity of 2 messages.

```ruby
ch = Concurrent::Promises::Channel.new 2
# => #<Concurrent::Promises::Channel:0x000002 capacity taken 0 of 2>
```

We push 3 messages, 
then it can be observed that the last thread pushing is sleeping 
since the channel is full. 

```ruby
threads = Array.new(3) { |i| Thread.new { ch.push message: i } } 
sleep 0.01 # let the threads run
threads
# => [#<Thread:0x000003@channel.in.md:14 dead>,
#     #<Thread:0x000004@channel.in.md:14 dead>,
#     #<Thread:0x000005@channel.in.md:14 sleep_forever>]
```

When message is popped the last thread continues and finishes as well.

```ruby
ch.pop                                   # => {:message=>1}
threads.map(&:join)
# => [#<Thread:0x000003@channel.in.md:14 dead>,
#     #<Thread:0x000004@channel.in.md:14 dead>,
#     #<Thread:0x000005@channel.in.md:14 dead>]
```

Same principle applies to popping as well.
There are now 2 messages int he channel.
Lets create 3 threads trying to pop a message, 
one will be blocked until new messages is pushed.

```ruby
threads = Array.new(3) { |i| Thread.new { ch.pop } } 
sleep 0.01 # let the threads run
threads 
# => [#<Thread:0x000006@channel.in.md:32 dead>,
#     #<Thread:0x000007@channel.in.md:32 dead>,
#     #<Thread:0x000008@channel.in.md:32 sleep_forever>]
ch.push message: 3
# => #<Concurrent::Promises::Channel:0x000002 capacity taken 0 of 2>
threads.map(&:value)
# => [{:message=>0}, {:message=>2}, {:message=>3}]
```

### Promises integration

However this channel is implemented to **integrate with promises**
therefore all operations can be represented as futures.

```ruby
ch = Concurrent::Promises::Channel.new 2
# => #<Concurrent::Promises::Channel:0x000009 capacity taken 0 of 2>
push_operations = Array.new(3) { |i| ch.push_op message: i }
# => [#<Concurrent::Promises::Future:0x00000a fulfilled with #<Concurrent::Promises::Channel:0x000009 capacity taken 2 of 2>>,
#     #<Concurrent::Promises::Future:0x00000b fulfilled with #<Concurrent::Promises::Channel:0x000009 capacity taken 2 of 2>>,
#     #<Concurrent::Promises::ResolvableFuture:0x00000c pending>]
```

> We do not have to sleep here letting the futures execute as Threads.
Since there is capacity for 2 messages the Promises are immediately resolved 
without ever allocating a Thread to execute. 
Push and pop operations are often more efficient.
The remaining pending push operation will also never require another thread,
instead it will resolve when a message is popped from the channel
making a space for a new message.    

```ruby
ch.pop_op.value!                         # => {:message=>0}
push_operations.map(&:value!)
# => [#<Concurrent::Promises::Channel:0x000009 capacity taken 2 of 2>,
#     #<Concurrent::Promises::Channel:0x000009 capacity taken 2 of 2>,
#     #<Concurrent::Promises::Channel:0x000009 capacity taken 2 of 2>]

pop_operations = Array.new(3) { |i| ch.pop_op }
# => [#<Concurrent::Promises::ResolvableFuture:0x00000d fulfilled with {:message=>1}>,
#     #<Concurrent::Promises::ResolvableFuture:0x00000e fulfilled with {:message=>2}>,
#     #<Concurrent::Promises::ResolvableFuture:0x00000f pending>]
ch.push message: 3 # (push|pop) can be freely mixed with (push_o|pop_op)
pop_operations.map(&:value) 
# => [{:message=>1}, {:message=>2}, {:message=>3}]
```

### Selecting over channels

A selection over channels can be created with the `.select_channel` factory method. It
will be fulfilled with a first message available in any of the channels. It
returns a pair to be able to find out which channel had the message available.

```ruby
ch1    = Concurrent::Promises::Channel.new 2
# => #<Concurrent::Promises::Channel:0x000010 capacity taken 0 of 2>
ch2    = Concurrent::Promises::Channel.new 2
# => #<Concurrent::Promises::Channel:0x000011 capacity taken 0 of 2>
ch1.push 1 
# => #<Concurrent::Promises::Channel:0x000010 capacity taken 1 of 2>
ch2.push 2 
# => #<Concurrent::Promises::Channel:0x000011 capacity taken 1 of 2>

Concurrent::Promises::Channel.select([ch1, ch2])
# => [#<Concurrent::Promises::Channel:0x000010 capacity taken 0 of 2>, 1]
ch1.select(ch2)
# => [#<Concurrent::Promises::Channel:0x000011 capacity taken 0 of 2>, 2]

Concurrent::Promises.future { 3 + 4 }.then_channel_push(ch1)
# => #<Concurrent::Promises::Future:0x000012 pending>
Concurrent::Promises::Channel. 
    # or `ch1.select_op(ch2)` would be equivalent
    select_op([ch1, ch2]).
    then('got number %03d from ch%d') { |(channel, value), format| 
      format format, value, [ch1, ch2].index(channel).succ
    }.value!                             # => "got number 007 from ch1"
```

### `try_` variants

All blocking operations ({#pop}, {#push}, {#select}) have non-blocking variant
with `try_` prefix. 
They always return immediately and indicate either success or failure.

```ruby
ch
# => #<Concurrent::Promises::Channel:0x000009 capacity taken 0 of 2>
ch.try_push 1                            # => true
ch.try_push 2                            # => true
ch.try_push 3                            # => false
ch.try_pop                               # => 1
ch.try_pop                               # => 2
ch.try_pop                               # => nil
```

### Timeouts

All blocking operations ({#pop}, {#push}, {#select}) have a timeout option.
Similar to `try_` variants it will indicate success or timing out, 
when the timeout option is used.

```ruby
ch
# => #<Concurrent::Promises::Channel:0x000009 capacity taken 0 of 2>
ch.push 1, 0.01                          # => true
ch.push 2, 0.01                          # => true
ch.push 3, 0.01                          # => false
ch.pop 0.01                              # => 1
ch.pop 0.01                              # => 2
ch.pop 0.01                              # => nil
```

### Backpressure

Most importantly the channel can be used to create systems with backpressure.
A self adjusting system where the producers will slow down 
if the consumers are not keeping up.

```ruby
channel = Concurrent::Promises::Channel.new 2
# => #<Concurrent::Promises::Channel:0x000013 capacity taken 0 of 2>
log     = Concurrent::Array.new          # => []

producers = Array.new 2 do |i|
  Thread.new(i) do |i|
    4.times do |j|
      log.push format "producer %d pushing %d", i, j      
      channel.push [i, j]      
    end
  end
end
# => [#<Thread:0x000014@channel.in.md:133 run>,
#     #<Thread:0x000015@channel.in.md:133 run>]

consumers = Array.new 4 do |i|
  Thread.new(i) do |consumer|
    2.times do |j|
      from, message = channel.pop
      log.push format "consumer %d got %d. payload %d from producer %d", 
                      consumer, j, message, from       
      do_stuff      
    end
  end
end
# => [#<Thread:0x000016@channel.in.md:142 run>,
#     #<Thread:0x000017@channel.in.md:142 run>,
#     #<Thread:0x000018@channel.in.md:142 run>,
#     #<Thread:0x000019@channel.in.md:142 run>]

# wait for all to finish
producers.map(&:join)
# => [#<Thread:0x000014@channel.in.md:133 dead>,
#     #<Thread:0x000015@channel.in.md:133 dead>]
consumers.map(&:join)
# => [#<Thread:0x000016@channel.in.md:142 dead>,
#     #<Thread:0x000017@channel.in.md:142 dead>,
#     #<Thread:0x000018@channel.in.md:142 dead>,
#     #<Thread:0x000019@channel.in.md:142 dead>]
# investigate log
log
# => ["producer 0 pushing 0",
#     "producer 0 pushing 1",
#     "producer 0 pushing 2",
#     "producer 1 pushing 0",
#     "consumer 0 got 0. payload 0 from producer 0",
#     "producer 0 pushing 3",
#     "consumer 1 got 0. payload 1 from producer 0",
#     "consumer 2 got 0. payload 2 from producer 0",
#     "consumer 3 got 0. payload 0 from producer 1",
#     "producer 1 pushing 1",
#     "producer 1 pushing 2",
#     "consumer 1 got 1. payload 3 from producer 0",
#     "producer 1 pushing 3",
#     "consumer 2 got 1. payload 1 from producer 1",
#     "consumer 3 got 1. payload 2 from producer 1",
#     "consumer 0 got 1. payload 3 from producer 1"]
```

The producers are much faster than consumers 
(since they `do_stuff` which takes some time)  
but as it can be seen from the log they fill the channel 
and then they slow down 
until there is space available in the channel.

If permanent allocation of threads to the producers and consumers has to be avoided,
the threads can be replaced with promises
that run a thread pool.

```ruby
channel = Concurrent::Promises::Channel.new 2
# => #<Concurrent::Promises::Channel:0x00001a capacity taken 0 of 2>
log     = Concurrent::Array.new          # => []

def produce(channel, log, producer, i)
  log.push format "producer %d pushing %d", producer, i      
  channel.push_op([producer, i]).then do
    i + 1 < 4 ? produce(channel, log, producer, i + 1) : :done    
  end      
end                                      # => :produce

def consume(channel, log, consumer, i)
  channel.pop_op.then(consumer, i) do |(from, message), consumer, i|
    log.push format "consumer %d got %d. payload %d from producer %d", 
                    consumer, i, message, from       
    do_stuff
    i + 1 < 2 ? consume(channel, log, consumer, i + 1) : :done       
  end
end                                      # => :consume

producers = Array.new 2 do |i|
  Concurrent::Promises.future(channel, log, i) { |*args| produce *args, 0 }.run
end
# => [#<Concurrent::Promises::Future:0x00001b pending>,
#     #<Concurrent::Promises::Future:0x00001c pending>]

consumers = Array.new 4 do |i|
  Concurrent::Promises.future(channel, log, i) { |*args| consume *args, 0 }.run
end
# => [#<Concurrent::Promises::Future:0x00001d pending>,
#     #<Concurrent::Promises::Future:0x00001e pending>,
#     #<Concurrent::Promises::Future:0x00001f pending>,
#     #<Concurrent::Promises::Future:0x000020 pending>]

# wait for all to finish
producers.map(&:value!)                  # => [:done, :done]
consumers.map(&:value!)                  # => [:done, :done, :done, :done]
# investigate log
log
# => ["producer 0 pushing 0",
#     "producer 1 pushing 0",
#     "producer 1 pushing 1",
#     "consumer 1 got 0. payload 0 from producer 1",
#     "consumer 2 got 0. payload 1 from producer 1",
#     "producer 0 pushing 1",
#     "producer 0 pushing 2",
#     "producer 0 pushing 3",
#     "producer 1 pushing 2",
#     "consumer 0 got 0. payload 0 from producer 0",
#     "consumer 3 got 0. payload 1 from producer 0",
#     "producer 1 pushing 3",
#     "consumer 2 got 1. payload 2 from producer 0",
#     "consumer 1 got 1. payload 3 from producer 0",
#     "consumer 3 got 1. payload 3 from producer 1",
#     "consumer 0 got 1. payload 2 from producer 1"]
```

### Synchronization of workers by passing a value

If the capacity of the channel is zero 
then any push operation will succeed only 
when there is a matching pop operation
which can take the message.
The operations have to be paired to succeed. 

```ruby
channel = Concurrent::Promises::Channel.new 0
# => #<Concurrent::Promises::Channel:0x000021 capacity taken 0 of 0>
thread = Thread.new { channel.pop }; sleep 0.01 
# allow the thread to go to sleep
thread
# => #<Thread:0x000022@channel.in.md:214 sleep_forever>
# succeeds because there is matching pop operation waiting in the thread 
channel.try_push(:v1)                    # => true
# remains pending, since there is no matching operation 
push = channel.push_op(:v2)
# => #<Concurrent::Promises::ResolvableFuture:0x000023 pending>
thread.value                             # => :v1
# the push operation resolves as a pairing pop is called
channel.pop                              # => :v2
push
# => #<Concurrent::Promises::ResolvableFuture:0x000023 fulfilled with #<Concurrent::Promises::Channel:0x000021 capacity taken 0 of 0>>
```
