# Final report

I started working on the project 6. Dec
and I have continued working on the project until 8. February as planned. 
I have worked on following abstractions Throttle, Cancellation, Channel, and ErlangActor.

The code developed during this project is available in 
<https://github.com/ruby-concurrency/concurrent-ruby/pull/791>.
The documentation is available at
<http://ruby-concurrency.github.io/concurrent-ruby/master/index.html>. 

## Throttle

The Throttle implementation originally had special APIs 
to interact with other abstractions like promises. 
However it was impractical and the API felt cumbersome.
Therefore the Throttle was finalized with much smaller API surface.
Capacity can be still directly acquired from the Throttle 
and then released.

The more common usage of the Throttle is with a proxy executor 
`a_throttle.on(Concurrent.global_io_executor)`. 
Anything executed on the proxy executor will be throttled and 
execute on the given executor. There can be more than one proxy executors.
All abstractions which execute tasks have option to specify executor, 
therefore the proxy executor can be injected to any abstraction 
throttling its concurrency level.

The abstraction is released in `concurrent-ruby-edge-0.5.0`. 
For more details see the documentation 
<http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Throttle.html>.

## Cancellation

The Cancellation abstraction provides cooperative cancellation.

The Cancellation abstraction was originally consisting of 2 classes,
during its finalization it was however simplified 
to be just a combination of Cancellation object 
and an origin which is regular Event or Future,
which improves compose-ability greatly. 
Any Event or Future can be easily turned into cancellation.    

The standard methods `Thread#raise` of `Thread#kill` available in Ruby
are very dangerous (see linked the blog posts bellow).
Therefore concurrent-ruby provides an alternative.
* <https://jvns.ca/blog/2015/11/27/why-rubys-timeout-is-dangerous-and-thread-dot-raise-is-terrifying/>
* <http://www.mikeperham.com/2015/05/08/timeout-rubys-most-dangerous-api/>
* <http://blog.headius.com/2008/02/rubys-threadraise-threadkill-timeoutrb.html>

It provides an object which represents a cancellation event 
which can be shared between tasks.
The task has to get the reference to the object 
and periodically cooperatively check that it is not cancelled.

The abstraction is released in `concurrent-ruby-edge-0.5.0`. 
For more details see the documentation 
<http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Cancellation.html>.

## Channel

The channel implementation is inspired by Go. 
However this implementation is more flexible. 
It has 3 major operations pop, push and select as expected.
Where each operation has 3 variants. 
`try_(pop|push|select)` which never blocks and returns always immediately.
`(pop|push|select)` which blocks current thread until it can be done 
or until it times out.
`(pop|push|select)_op` which returns Future representing the operation,
which can be easily composed with other asynchronous tasks.

The abstraction is released in `concurrent-ruby-edge-0.5.0`. 
For more details see the documentation
<http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/Promises/Channel.html>. 

## Erlang actors

The actor implementation matches the Erlang's implementation.
The Erlang compatible implementation was chosen for two reasons. 
First reason was to make porting of the Erlang's
[OTP](https://learnyousomeerlang.com/what-is-otp) library possible.
OTP is time proven library and even a philosophy how to write reliable concurrent applications. 
Second reason was 
that there is an intersection between Ruby and Elixir programmers.
Elixir runs on Erlang's VM and the programmers are familiar with OTP,
therefore they will be able to reuse their knowledge in Ruby.    
 
Mainly: 

*   The `exit/1` and `exit/2` 
    functions are reimplemented with the same arguments and effects. 
    Even though the methods are named `terminate` to avoid collision with `Kernel#exit`.r
    This re-implementation adds that the termination event not only sends signals to linked actors 
    but it is represented as a Future 
    which is fulfilled with the final value of the actor or rejected with the reason of abnormal termination.

*   The linking and monitoring between actors (called processes in Erlang) is re-implemented.
    Functions `link`, `unlink`, `spawn_link`, `monitor`, `demonitor`, `spawn_monitor`
    have equivalent counterparts 
    `link`, `unlink`, `spawn link:true`, `monitor`, `demonitor`, `spawn monitor: true`.
    All the options applicable in this implementation are supported, 
    they effects are the same 
    and the ordering of signal and messages is the same as on Erlang.

*   This implementation has two functionally equivalent types of actors
    `spawn type: on_thread, ...` and `spawn type: on_pool, ...`. 
    They differ in the execution mode. 
    First requires its own thread second runs on shared thread pool
    therefore allowing to have millions of short or long lived actors if required.
 
*   Message and signal ordering of messages between two actors has same guarantee as in Erlang.
    Messages and signals from A are always received by B in the order they were send. 
    (Signals are internal messages produced by methods like `link`.)
    The ordering guarantee does not scale up to more than 2 actors in Erlang nor in this implementation.

*   Even though Ruby does not have pattern matching, this implementation provides `receive`
    which is functionally equivalent. 
    (It is sometimes more cumbersome to use though.)

Exit behaviour, linking, and monitoring is very well described by 
[the chapter of the book "learn you some Erlang"](https://learnyousomeerlang.com/errors-and-processes).
This implementation matches the behaviours described there.

Erlang method documentation can be found at 
<http://erlang.org/documentation/doc-10.3/erts-10.3/doc/html/erlang.html>. 

### Actor execution modes - types

The actors can be written in 2 modes. First will require it's own thread, 
second will run on a thread pool. 
Please see 
[Actor types section](http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/ErlangActor.html)
for more details.

### Ordering

Especially ordering guarantees are not easy to get correct. 
As an example lets have a look at the reasoning behind implementation of monitoring. 
Understanding of the monitors in Erlang actors is necessary for the following part.

When `#monitor` is called in actor A it sends a Monitor signal to actor B.
The actor B will then send a Down signal to A when it terminates.
Actor is not processing any messages or signals when after it terminates.
Therefore the monitor method needs to also check if B terminated.

Lets reason about the ordering between sending the signal Monitor and checking termination of B.
If A first checks termination of B sending Monitor signal only if B is not terminated
then A can never get a reply if B terminates after A checks its termination and before A sends Monitor signal.
Therefore A needs to first optimistically send a Monitor signal and then check if B terminated.
If B already terminated then we do not expect it to send a Down signal, 
instead the `#monitor` places Down message with reason NoActor immediately into A's mailbox.

We will now move our focus to B considering the case when A send the signal
and the termination check of B was false.
The normal case is that B gets the Monitor signal and processes it 
remembering it is monitored.
Then on termination B sends a Down signal with the reason for termination to A.
The more interesting case is when the actor B gets the Monitor signal into its mailbox
but it is terminated before it can process it. 
In that case,
since we know that A did no see B terminated,
we have to process the Monitor signal even if terminated and send a corresponding Down signal to A.
Therefore the B actor termination does two main operations in the following order:
it resolves its termination future (terminates) which is used by A in monitor to do the check,
it drains its mailbox looking for signals which have to be replied to.
The mailbox draining has to happen after termination is resolved 
otherwise it could happen before A sends its Monitor signal which could then go unanswered.
    
    B drains > A sends Monitor signal > A termination check false > B terminates
    # the Monitor signal is never processed by B

Therefore we have concluded that A has send the monitor signal first 
then check B's termination and B has to terminate first 
(resolve its termination future) then drain signals from mailbox.
With this ordering following cases can happen:

    A sends Monitor signal > A termination check false > B terminates > B drains and replies    
    A sends Monitor signal > B terminates > A termination check true therefore A places Down itself
    B terminates > A sends Monitor signal > A termination check true therefore A places Down itself    

Where in each case A gets the Down message.

There is one last problem which could happen, 
the Down message could be received twice by A.
It could happen in the last two sequences 
where A detects B's termination 
and where we did not consider B's drain for simplicity.
The last two sequences should actually be: 
 
    A sends Monitor signal > B terminates > A termination check true therefore A places Down itself > B drains and replies
    B terminates > A sends Monitor signal > A termination check true therefore A places Down itself > B drains and replies    
    A sends Monitor signal > B terminates > B drains and replies > A termination check true therefore A places Down itself 
    B terminates > A sends Monitor signal > B drains and replies > A termination check true therefore A places Down itself     
    B terminates > B drains > A sends Monitor signal > A termination check true therefore A places Down itself     
   
In the first four orderings the drain is happening after monitor call sends Monitor signal in A
therefore the draining will send Down signal 
because it cannot know if A send itself Down message about B's termination.
The A actor has to prevent the duplication.
In its state it stores an information about the active monitors (set by the `#monitor`),
when a Down message arrives it is deleted
therefore any subsequent Down messages are ignored.
Both monitor call in A and the draining in B sends Down signal with a NoActor reason
so it does not matter which arrives first.

This was a reasoning for the actor monitoring implementation. 
Other actor features like linking, demonitoring, etc. required similar approach.

The abstraction is ready for release. 
For more details about usage see the API documentation 
<http://ruby-concurrency.github.io/concurrent-ruby/master/Concurrent/ErlangActor.html>.

## Integration

Integration of concurrency abstractions was a motivation of the project.
I've added Promises library to the concurrent-ruby in the past
which can represent future computations and values 
and therefore can be used as a connecting element between abstractions.

```ruby
an_actor.ask_op(:payload).then_flat { |reply| a_channel.push_op reply }
```

In the example above an actor is asked with a payload, 
which is represented as a Future object. 
When the Future is resolved with a reply 
it executes the block with the reply argument
usually defined by `then` method.
In this case `then_flat` needs to be used
because we want a Future representing the value of the inner push operation 
pushing the reply into a channel.
All the operations in this example are done asynchronously on a thread pool.

Usual direct thread blocking mode is also always supported. 
The following example does the same but uses the current Thread to do the work. 

```ruby
reply = an_actor.ask(:payload) # blocks until it replies
a_channel.push reply # blocks if there is no space in the channel. 
```

In addition all blocking operations support timeouts, 
since it is a good practice to give each blocking operation a timeout 
and try to recover if it takes too long.
It usually prevents the whole application from hitting a deadlock, 
or at least it can give developer idea what is going wrong 
if timeouts are logged.

Promises are also used instead of state flags.
So for example termination of actor is not implemented as simple `#terminated? #=> true or false` method
but `#terminated` returns a future which is resolved when the Actor terminates.
More over if it is fulfilled it means actor terminated normally with a `actor.terminated.value`
and when it is rejected it means that the actor terminated abnormally because of `actor.terminated.reason`.
That again allows to integrate with other abstractions, e.g.

```ruby
actor.terminated.value! # block current thread until actor terminates or raise reason if any
actor.terminated.then(actor) { |value, actor| a_logger.debug "#{actor} terminated with #{value}" }
```    
   
Besides chaining and connecting abstractions together,
concurrency level of all abstractions executing tasks can be manages with the Throttle abstraction.

```ruby
throttle = Concurrent::Throttle.new 10
1000.times do
  Thread.new do
    actor = Concurrent::ErlangActor.spawn_actor type: :on_pool, executor: throttle.on(:io) do
      receive(keep: true) { |m| reply m }  
    end
    actor.ask :ping 
    Concurrent::Promises.future_on(throttle.on(:fast)) { 1 + 1 }.then(&:succ)
  end
end
```

In the example above the throttle ensures that
there is at most 10 actors or futures processing message or executing their bodies.
Notice that this works not only across abstractions but also across thread pools.
The actor is running on the global thread pool for blocking operations - `:io`
and the futures are executing on the global thread poll for `:fast` non-blocking operations.

This is of course not an exhaustive list of possible ways how the abstractions can be integrated
but rather few examples to give a feeling what is possible.
Please also see an executable 
[example](http://ruby-concurrency.github.io/concurrent-ruby/master/file.medium-example.out.html)
using the integrations.

## What was not finished

The original proposal also contained a work steeling thread pool 
which would improve performance of small non-blocking tasks.
It would not provide any new functionality to the users. 
Therefore for lack of time I decided to postpone this for some later time. 

## Release

All the work done during the project is released in `concurrent-ruby-edge` version 0.5.0 to Ruby users. 
After some time when feedback is gathered the abstractions will be released in the stable core - `concurrent-ruby`.
This is necessary because anything released in the core has to stay backward compatible,
therefore it would prevent even minor improvements to the API.
No big changes to the APIs are expected.

## After the project

During the project it become apparent that there will not be much time left 
to focus on propagation of the new abstractions. 
I've rather decided to focus on the abstraction development  
and completion of all their API documentation.

I plan to turn my attention 
to letting Ruby community know about the project and the new features after the project ends.
I will record four introductory videos for each abstraction, 
since it appears to me that it become a better platform to reach wider audience then writing blog posts.
