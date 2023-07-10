## Examples

The simplest example is to use the actor as an asynchronous execution.
Although, `Promises.future { 1 + 1 }` is better suited for that purpose.

```ruby
actor = Concurrent::ErlangActor.spawn(type: :on_thread, name: 'addition') { 1 + 1 }
actor.terminated.value!
```

Let's send some messages and maintain some internal state 
which is what actors are good for.

```ruby
actor = Concurrent::ErlangActor.spawn(type: :on_thread, name: 'sum') do
  sum = 0 # internal state
  # receive and sum the messages until the actor gets :done
  while true
    message = receive
    break if message == :done
    # if the message is asked and not only told, 
    # reply with the current sum (has no effect if actor was not asked)
    reply sum += message   
  end
  # The final value of the actor
  sum
end
```

The actor can be either told a message asynchronously, 
or asked. The ask method will block until actor replies.

```ruby
# tell returns immediately returning the actor 
actor.tell(1).tell(1)
# blocks, waiting for the answer 
actor.ask 10
# stop the actor
actor.tell :done
# The final value of the actor 
actor.terminated.value!
```

### Actor types

There are two types of actors. 
The type is specified when calling spawn as a first argument, 
`Concurrent::ErlangActor.spawn(type: :on_thread, ...` or 
`Concurrent::ErlangActor.spawn(type: :on_pool, ...`.

The main difference is in how receive method returns.
 
-   `:on_thread` it blocks the thread until message is available, 
    then it returns or calls the provided block first. 
 
-   However, `:on_pool` it has to free up the thread on the receive 
    call back to the pool. Therefore the call to receive ends the 
    execution of current scope. The receive has to be given block
    or blocks that act as a continuations and are called 
    when there is message available.
 
Let's have a look at how the bodies of actors differ between the types:

```ruby
ping = Concurrent::ErlangActor.spawn(type: :on_thread) { reply receive }
ping.ask 42
```

It first calls receive, which blocks the thread of the actor. 
When it returns the received message is passed an an argument to reply,
which replies the same value back to the ask method. 
Then the actor terminates normally, because there is nothing else to do.

However when running on pool a block with code which should be evaluated 
after the message is received has to be provided. 

```ruby
ping = Concurrent::ErlangActor.spawn(type: :on_pool) { receive { |m| reply m } }
ping.ask 42
```

It starts by calling receive which will remember the given block for later
execution when a message is available and stops executing the current scope.
Later when a message becomes available the previously provided block is given
the message and called. The result of the block is the final value of the 
normally terminated actor.

The direct blocking style of `:on_thread` is simpler to write and more straight
forward however it has limitations. Each `:on_thread` actor creates a Thread 
taking time and resources. 
There is also a limited number of threads the Ruby process can create 
so you may hit the limit and fail to create more threads and therefore actors.  

Since the `:on_pool` actor runs on a poll of threads, its creations 
is faster and cheaper and it does not create new threads. 
Therefore there is no limit (only RAM) on how many actors can be created.

To simplify, if you need only few actors `:on_thread` is fine. 
However if you will be creating hundreds of actors or 
they will be short-lived `:on_pool` should be used.      

### Receiving messages

Simplest message receive.

```ruby
actor = Concurrent::ErlangActor.spawn(type: :on_thread) { receive }
actor.tell :m
actor.terminated.value!
```

which also works for actor on pool, 
because if no block is given it will use a default block `{ |v| v }` 

```ruby
actor = Concurrent::ErlangActor.spawn(type: :on_pool) { receive { |v| v } }
# can simply be following
actor = Concurrent::ErlangActor.spawn(type: :on_pool) { receive }
actor.tell :m
actor.terminated.value!
```

The received message type can be limited.

```ruby
Concurrent::ErlangActor.
  spawn(type: :on_thread) { receive(Numeric).succ }.
  tell('junk'). # ignored message
  tell(42).
  terminated.value!
```

On pool it requires a block.

```ruby
Concurrent::ErlangActor.
  spawn(type: :on_pool) { receive(Numeric) { |v| v.succ } }.
  tell('junk'). # ignored message
  tell(42).
  terminated.value!
```

By the way, the body written for on pool actor will work for on thread actor 
as well. 

```ruby
Concurrent::ErlangActor.
  spawn(type: :on_thread) { receive(Numeric) { |v| v.succ } }.
  tell('junk'). # ignored message
  tell(42).
  terminated.value!
```

The `receive` method can be also used to dispatch based on the received message.

```ruby
actor = Concurrent::ErlangActor.spawn(type: :on_thread) do
  while true
    receive(on(Symbol) { |s| reply s.to_s },
            on(And[Numeric, -> v { v >= 0 }]) { |v| reply v.succ },
            # put last works as else
            on(ANY) do |v| 
              reply :bad_message
              terminate [:bad_message, v]
            end)            
  end 
end
actor.ask 1
actor.ask 2
actor.ask :value
# this malformed message will terminate the actor
actor.ask -1
# the actor is no longer alive, so ask fails
actor.ask "junk" rescue $!
actor.terminated.result
```

And a same thing for the actor on pool. 
Since it cannot loop it will call the body method repeatedly.

```ruby
module Behaviour
  def body
    receive(on(Symbol) do |s| 
              reply s.to_s 
              body # call again  
            end,
            on(And[Numeric, -> v { v >= 0 }]) do |v| 
              reply v.succ
              body # call again 
            end,
            # put last works as else
            on(ANY) do |v| 
              reply :bad_message
              terminate [:bad_message, v]
            end)  
  end
end

actor = Concurrent::ErlangActor.spawn(type: :on_pool, environment: Behaviour) { body }
actor.ask 1
actor.ask 2
actor.ask :value
# this malformed message will terminate the actor
actor.ask -1
# the actor is no longer alive, so ask fails
actor.ask "junk" rescue $!
actor.terminated.result
```

Since the behavior is stable in this case we can simplify with the `:keep` option
that will keep the receive rules until another receive is called
replacing the kept rules.

```ruby
actor = Concurrent::ErlangActor.spawn(type: :on_pool) do
  receive(on(Symbol) { |s| reply s.to_s },
          on(And[Numeric, -> v { v >= 0 }]) { |v| reply v.succ },
          # put last works as else
          on(ANY) do |v| 
            reply :bad_message
            terminate [:bad_message, v]
          end,
          keep: true)            
end
actor.ask 1
actor.ask 2
actor.ask :value
# this malformed message will terminate the actor
actor.ask -1
# the actor is no longer alive, so ask fails
actor.ask "junk" rescue $!
actor.terminated.result
```

### Erlang behaviour

The actor matches Erlang processes in behaviour. 
Therefore it supports the usual Erlang actor linking, monitoring, exit behaviour, etc.

```ruby
actor = Concurrent::ErlangActor.spawn(type: :on_thread) do
  spawn(link: true) do # equivalent of spawn_link in Erlang
    terminate :err # equivalent of exit in Erlang    
  end
  trap # equivalent of process_flag(trap_exit, true) 
  receive  
end
actor.terminated.value!
```

The methods have same or very similar name to be easily found. 
The one exception from the original Erlang naming is exit.
To avoid clashing with `Kernel#exit` it's called `terminate`. 

Until there is more information available here, the chapters listed below from 
a book [lern you some Erlang](https://learnyousomeerlang.com) 
are excellent source of information. 
The Ruby ErlangActor implementation has same behaviour. 

-   [Links](https://learnyousomeerlang.com/errors-and-processes#links)
-   [It's a trap](https://learnyousomeerlang.com/errors-and-processes#its-a-trap)
-   [Monitors](https://learnyousomeerlang.com/errors-and-processes#monitors)

If anything behaves differently than in Erlang, please file an issue.

### Chapters or points to be added

*   More erlang behaviour examples.
*   The mailbox can be bounded in size, 
    then the tell and ask will block until there is space available in the mailbox.
    Useful for building systems with backpressure.
*   `#tell_op` and `ask_op` method examples, integration with promises.
*   Best practice: always use timeout, 
    and do something if the message does not arrive, don't leave the actor stuck.
*   Best practice: drop and log unrecognized messages, 
    or be even more defensive and terminate.
*   Environment definition for actors.
