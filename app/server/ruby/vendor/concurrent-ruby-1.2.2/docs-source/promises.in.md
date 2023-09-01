# Basics

## Factory methods

Future and Event are created indirectly with constructor methods in
FactoryMethods. They are not designed for inheritance but rather for
composition.

```ruby
Concurrent::Promises::FactoryMethods.instance_methods(false).sort
```

The module can be included or extended where needed.

```ruby
Class.new do
  include Concurrent::Promises::FactoryMethods

  def a_method
    resolvable_event
  end
end.new.a_method

mod = Module.new do
  extend Concurrent::Promises::FactoryMethods
end #
mod.resolvable_event
```

The default executor can be changed by overriding `default_executor` method
inherited from `Concurrent::Promises::FactoryMethods`.

```ruby
mod = Module.new do
  extend Concurrent::Promises::FactoryMethods
  def self.default_executor
    :fast
  end
end #
mod.future { 1 }.default_executor
Concurrent::Promises.future { 1 }.default_executor
```


The module is already extended into {Concurrent::Promises} for convenience.

```ruby
Concurrent::Promises.resolvable_event
```

## Asynchronous task

The most basic use-case of the framework is asynchronous processing. A task can
be processed asynchronously by using a `future` factory method. The block will
be executed on an internal thread pool.

Arguments of `future` are passed to the block and evaluation starts immediately.

```ruby
future = Concurrent::Promises.future(0.1) do |duration|
  sleep duration
  :result
end
future.value
```

Asks if the future is resolved, here it will be still in the middle of the
sleep call.

```ruby
future.resolved?
```

Retrieving the value will block until the future is **resolved**.

```ruby
future.value
future.resolved?
```

If the task fails, we talk about the future being **rejected**.

```ruby
future = Concurrent::Promises.future { sleep 0.01; raise 'Boom' }
```

There is no result, the future was rejected with a reason.

```ruby
future.value
future.reason
```

It can be forced to raise the reason for rejection when retrieving the value.

```ruby
begin
  future.value! 
rescue => e 
  e
end
```

Which is the same as `future.value! rescue $!` which will be used hereafter.

Or it can be used directly as argument for raise, since it implements exception
method.

```ruby
raise future rescue $!
```

## States

Let's define an inspection helper for methods.

```ruby
def inspect_methods(*methods, of:)
  methods.reduce({}) { |h, m| h.update m => of.send(m) }
end #
```

Event has a `pending` and a `resolved` state. 

```ruby
event = Concurrent::Promises.resolvable_event #
inspect_methods(:state, :pending?, :resolved?, of: event)

event.resolve #
inspect_methods(:state, :pending?, :resolved?, of: event)
```

Future's `resolved` state is further specified to be `fulfilled` or `rejected`.

```ruby
future = Concurrent::Promises.resolvable_future #
inspect_methods(:state, :pending?, :resolved?, :fulfilled?, :rejected?, 
    of: future)

future.fulfill :value #
inspect_methods(:state, :pending?, :resolved?, :fulfilled?, :rejected?,
    :result, :value, :reason, of: future)

future = Concurrent::Promises.rejected_future StandardError.new #
inspect_methods(:state, :pending?, :resolved?, :fulfilled?, :rejected?, 
    :result, :value, :reason, of: future)
```

## Direct creation of resolved futures

When an existing value has to be wrapped in a future it does not have to go
through evaluation as follows.

```ruby
Concurrent::Promises.future { sleep 0.01; :value }
```

Instead, it can be created directly as already-resolved:

```ruby
Concurrent::Promises.fulfilled_future(:value)
Concurrent::Promises.rejected_future(StandardError.new('Ups'))
Concurrent::Promises.resolved_future(true, :value, nil)
Concurrent::Promises.resolved_future(false, nil, StandardError.new('Ups'))
```

## Chaining

A big advantage of promises is the ability to chain tasks together without blocking
the current thread.

```ruby
Concurrent::Promises.
    future(2) { |v| v.succ }.
    then(&:succ).
    value!
```

As `future` factory method takes an argument, so does the `then` method. Any
supplied arguments are passed to the block, and the library ensures that they
are visible to the block.

```ruby
Concurrent::Promises.
    future('3') { |s| s.to_i }.
    then(2) { |v, arg| v + arg }.
    value
Concurrent::Promises.
    fulfilled_future('3').
    then(&:to_i).
    then(2, &:+).
    value
Concurrent::Promises.
    fulfilled_future(1).
    chain(2) { |fulfilled, value, reason, arg| value + arg }.
    value
```

Passing the arguments in (similarly as for a thread `Thread.new(arg) { |arg|
do_stuff arg }`) is **required**. Both of the following bad examples may break:

```ruby
arg = 1
Thread.new { do_stuff arg }
Concurrent::Promises.future { do_stuff arg }
```

Correct:

```ruby
arg = 1
Thread.new(arg) { |arg| do_stuff arg }
Concurrent::Promises.future(arg) { |arg| do_stuff arg }
```

## Branching, and zipping

Besides chaining it can also be branched.

```ruby
head    = Concurrent::Promises.fulfilled_future -1 #
branch1 = head.then(&:abs) #
branch2 = head.then(&:succ).then(&:succ) #

branch1.value!
branch2.value!
```

It can be combined back to one future by zipping (`zip`, `&`).

```ruby
branch1.zip(branch2).value!
(branch1 & branch2).
    then { |a, b| a + b }.
    value!
(branch1 & branch2).
    then(&:+).
    value!
Concurrent::Promises.
    zip(branch1, branch2, branch1).
    then { |*values| values.reduce(&:+) }.
    value!
```

Instead of zipping only the first one can be taken, if needed.

```ruby
Concurrent::Promises.any(branch1, branch2).value!
(branch1 | branch2).value!
```

## Blocking methods

In these examples we have used blocking methods like `value` extensively for
their convenience, however in practice is better to avoid them and continue
chaining.

If they need to be used (e.g. when integrating with threads), `value!` is a
better option over `value` when rejections are not dealt with differently.
Otherwise the rejections are not handled and probably silently forgotten.

## Error handling

When a task in the chain fails, the rejection propagates down the
chain without executing the tasks created with `then`.

```ruby
Concurrent::Promises.
    fulfilled_future(Object.new).
    then(&:succ).
    then(&:succ).
    result
```

As `then` chained tasks execute only on fulfilled futures, there is a `rescue`
method which chains a task which is executed only when the future is rejected. 
It can be used to recover from rejection.

Using rescue to fulfill to 0 instead of the error.

```ruby
Concurrent::Promises.
    fulfilled_future(Object.new).
    then(&:succ).
    then(&:succ).
    rescue { |err| 0 }.
    result
```

Rescue not executed when there is no rejection.

```ruby
Concurrent::Promises.
    fulfilled_future(1).
    then(&:succ).
    then(&:succ).
    rescue { |e| 0 }. 
    result
```

Tasks added with `chain` are always evaluated.

```ruby
Concurrent::Promises.
    fulfilled_future(1).
    chain { |fulfilled, value, reason| fulfilled ? value : reason }.
    value!
Concurrent::Promises.
    rejected_future(StandardError.new('Ups')).
    chain { |fulfilled, value, reason| fulfilled ? value : reason }.
    value!
```

Zip is rejected if any of the zipped futures is.

```ruby
rejected_zip = Concurrent::Promises.zip(
    Concurrent::Promises.fulfilled_future(1),
    Concurrent::Promises.rejected_future(StandardError.new('Ups')))
rejected_zip.result
rejected_zip.
    rescue { |reason1, reason2| (reason1 || reason2).message }.
    value
```

## Delayed futures

Delayed futures will not evaluate until asked by `touch` or other method
requiring resolution. 

```ruby
future = Concurrent::Promises.delay { sleep 0.01; 'lazy' }
sleep 0.01 #
future.resolved?
future.touch
sleep 0.02 #
future.resolved?
```

All blocking methods like `wait`, `value` call `touch` and trigger evaluation.

```ruby
Concurrent::Promises.delay { :value }.value
```

It propagates up through the chain, allowing whole or partial lazy chains.

```ruby
head    = Concurrent::Promises.delay { 1 } #
branch1 = head.then(&:succ) #
branch2 = head.delay.then(&:succ) #
join    = branch1 & branch2 #

sleep 0.01 #
```

Nothing resolves.

```ruby
[head, branch1, branch2, join].map(&:resolved?)
```

Force `branch1` evaluation.

```ruby
branch1.value
sleep 0.01 #
[head, branch1, branch2, join].map(&:resolved?)
```

Force evaluation of both by calling `value` on `join`.

```ruby
join.value
[head, branch1, branch2, join].map(&:resolved?)
```

## Flatting

Sometimes it is needed to wait for an inner future. An apparent solution is to wait
inside the future `Concurrent::Promises.future { Concurrent::Promises.future { 1+1 }.value }.value`.
However, as mentioned before, `value` calls should be **avoided** to avoid
blocking threads. Therefore there is a `#flat` method which is a correct solution
in this situation and does not block any thread.

```ruby
Concurrent::Promises.future { Concurrent::Promises.future { 1+1 } }.flat.value!
```

A more complicated example.

```ruby
Concurrent::Promises.
    future { Concurrent::Promises.future { Concurrent::Promises.future { 1 + 1 } } }.
    flat(1).
    then { |future| future.then(&:succ) }.
    flat(1).
    value!
```

## Scheduling

Tasks can be planned to be executed with a time delay.

Schedule task to be executed in 0.1 seconds.

```ruby
scheduled = Concurrent::Promises.schedule(0.1) { 1 }
scheduled.resolved?
```

Value will become available after 0.1 seconds. 

```ruby
scheduled.value
```

It can be used in the chain as well, where the delay is counted from the moment
its parent resolves. Therefore, the following future will be resolved in 0.2 seconds.

```ruby
future = Concurrent::Promises.
    future { sleep 0.01; :result }.
    schedule(0.01).
    then(&:to_s).
    value!
```

Time can be used as well.

```ruby
Concurrent::Promises.schedule(Time.now + 10) { :val }
```

## Resolvable Future and Event:

Sometimes it is required to resolve a future externally, in these cases
`resolvable_future` and `resolvable_event` factory methods can be used. See
{Concurrent::Promises::ResolvableFuture} and
{Concurrent::Promises::ResolvableEvent}.

```ruby
future = Concurrent::Promises.resolvable_future
```

The thread will be blocked until the future is resolved

```ruby
thread = Thread.new { future.value } #
future.fulfill 1
thread.value
```

A future can be resolved only once.

```ruby
future.fulfill 1 rescue $!
future.fulfill 2, false
```

## How are promises executed?

Promises use global pools to execute the tasks. Therefore each task may run on
different threads which implies that users have to be careful not to depend on
Thread-local variables (or they have to be set at the beginning of the task and
cleaned up at the end of the task).

Since the tasks are running on may different threads of the thread pool, it's
better to follow following rules:

-   Use only data passed via arguments or values of parent futures, to 
    have better control over what are futures accessing.
-   The data passed in and out of futures is easier to deal with if it is 
    immutable or at least treated as such.
-   Any mutable and mutated object accessed by more than one thread or future 
    must be thread-safe, see {Concurrent::Array}, {Concurrent::Hash}, and 
    {Concurrent::Map}. (The value of a future may be consumed by many futures.)
-   Futures can access outside objects, but they have to be thread-safe.

> *TODO: This part to be extended*

# Advanced

## Callbacks

```ruby
queue  = Queue.new
future = Concurrent::Promises.delay { 1 + 1 }

future.on_fulfillment { queue << 1 } # evaluated asynchronously
future.on_fulfillment! { queue << 2 } # evaluated on resolving thread

queue.empty?
future.value
queue.pop
queue.pop
```

## Using executors

Factory methods, chain, and callback methods all have other versions of them
which takes an executor argument.

It takes an instance of an executor, or a symbol which is a shortcut for the
two global pools in concurrent-ruby. `:fast` for short and non-blocking tasks
and `:io` for long-running and blocking tasks.

```ruby
Concurrent::Promises.future_on(:fast) { 2 }.
    then_on(:io) { File.read __FILE__ }.
    value.size
```

## Run (simulated process)

Similar to flatting is running. When `run` is called on a future it will flat
indefinitely as long the future fulfils into a `Future` value. It can be used
to simulate a thread-like processing without actually occupying the thread.

```ruby
count = lambda do |v|
  v += 1
  v < 5 ? Concurrent::Promises.future_on(:fast, v, &count) : v
end
400.times.
    map { Concurrent::Promises.future_on(:fast, 0, &count).run.value! }.
    all? { |v| v == 5 }
```

Therefore the above example finished fine on the the `:fast` thread pool even
though it has much fewer threads than are simulated in the simulated process.

# Interoperability

## Actors

Create an actor which takes received numbers and returns the number squared. 

```ruby
actor = Concurrent::Actor::Utils::AdHoc.spawn :square do
  -> v { v ** 2 }
end
```

Send result of `1+1` to the actor, and add 2 to the result sent back from the
actor.

```ruby
Concurrent::Promises.
    future { 1 + 1 }.
    then_ask(actor).
    then { |v| v + 2 }.
    value!
```

So `(1 + 1)**2 + 2 = 6`.

The `ask` method returns future.

```ruby
actor.ask(2).then(&:succ).value!
```

## Channel

There is an implementation of channel as well. Let's start by creating a
channel with a capacity of 2 messages.

```ruby
ch1 = Concurrent::Promises::Channel.new 2
```

We push 3 messages, it can be observed that the last future representing the
push is not fulfilled since the capacity prevents it. When the work which fills
the channel depends on the futures created by push it can be used to create
backpressure – the filling work is delayed until the channel has space for
more messages.

```ruby
pushes = 3.times.map { |i| ch1.push_op i }
ch1.pop_op.value!
pushes
```

A selection over channels can be created with the `.select_channel` factory method. It
will be fulfilled with a first message available in any of the channels. It
returns a pair to be able to find out which channel had the message available.

```ruby
ch2    = Concurrent::Promises::Channel.new 2
result = Concurrent::Promises::Channel.select_op([ch1, ch2])
result.value!

Concurrent::Promises.future { 1+1 }.then_channel_push(ch1)
result = (
    Concurrent::Promises.fulfilled_future('%02d') &      
        Concurrent::Promises::Channel.select_op([ch1, ch2])).
    then { |format, (channel, value)| format format, value } #
result.value!
```

## ProcessingActor

There is also a new implementation of actors based on the Channel and the
ability of promises to simulate processes. The actor runs as a process but also
does not occupy a thread per actor as the previously-described Concurrent::Actor
implementation. This implementation is close to Erlang actors, therefore OTP
can be ported for this actors (and it's planned).

The simplest actor is one which just computes without even receiving a
message.

```ruby
actor = Concurrent::ProcessingActor.act(an_argument = 2) do |actor, number|
  number ** 3
end
actor.termination.value!
```
Let's receive some messages though.

```ruby
add_2_messages = Concurrent::ProcessingActor.act do |actor|
  # Receive two messages then terminate normally with the sum.
  (actor.receive & actor.receive).then do |a, b|
    a + b
  end
end
add_2_messages.tell_op 1
add_2_messages.termination.resolved?
add_2_messages.tell_op 3
add_2_messages.termination.value!
```

Actors can also be used to apply backpressure to a producer. Let's start by
defining an actor which a mailbox of size 2.

```ruby
slow_counter = -> (actor, count) do
  actor.receive.then do |command, number|
    sleep 0.01
    case command
    when :add
      slow_counter.call actor, count + number
    when :done
      # terminate
      count
    end
  end
end

actor = Concurrent::ProcessingActor.act_listening( 
    Concurrent::Promises::Channel.new(2), 
    0,
    &slow_counter)
```

Now we can create a producer which will push messages only when there is a
space available in the mailbox. We use promises to free a thread during waiting
on a free space in the mailbox.

```ruby
produce = -> receiver, i do
  if i < 10
    receiver.
        # send a message to the actor, resolves only after the message is 
        # accepted by the actor's mailbox
        tell_op([:add, i]).
        # send incremented message when the above message is accepted 
        then(i+1, &produce)
  else
    receiver.tell_op(:done)
    # do not continue 
  end
end

Concurrent::Promises.future(actor, 0, &produce).run.wait!

actor.termination.value!
```


# Use-cases

## Simple background processing
  
```ruby
Concurrent::Promises.future { do_stuff }
```

## Parallel background processing

```ruby
tasks = 4.times.map { |i| Concurrent::Promises.future(i) { |i| i*2 } }
Concurrent::Promises.zip(*tasks).value!
```

## Actor background processing

Actors are mainly keep and isolate state, they should stay responsive not being
blocked by a longer running computations. It desirable to offload the work to
stateless promises.

Lets define an actor which will process jobs, while staying responsive, and
tracking the number of tasks being processed.

```ruby
class Computer < Concurrent::Actor::RestartingContext
  def initialize
    super()
    @jobs = {}
  end

  def on_message(msg)
    command, *args = msg
    case command
    # new job to process
    when :run
      job        = args[0]
      @jobs[job] = envelope.future
      # Process asynchronously and send message back when done.
      Concurrent::Promises.future(&job).chain(job) do |fulfilled, value, reason, job|
        self.tell [:done, job, fulfilled, value, reason]
      end
      # Do not make return value of this method to be answer of this message.
      # We are answering later in :done by resolving the future kept in @jobs.
      Concurrent::Actor::Behaviour::MESSAGE_PROCESSED
    when :done
      job, fulfilled, value, reason = *args
      future                        = @jobs.delete job
      # Answer the job's result.
      future.resolve fulfilled, value, reason
    when :status
      { running_jobs: @jobs.size }
    else
      # Continue to fail with unknown message.
      pass 
    end
  end
end
```

Create the computer actor and send it 3 jobs.

```ruby
computer = Concurrent::Actor.spawn Computer, :computer
results = 3.times.map { computer.ask [:run, -> { sleep 0.01; :result }] }
computer.ask(:status).value!
results.map(&:value!)
```
## Solving the Thread count limit by thread simulation

Sometimes an application requires to process a lot of tasks concurrently. If
the number of concurrent tasks is high enough than it is not possible to create
a Thread for each of them. A partially satisfactory solution could be to use
Fibers, but that solution locks the application on MRI since other Ruby
implementations are using threads for each Fiber.

This library provides a {Concurrent::Promises::Future#run} method on a future
to simulate threads without actually accepting one all the time. The run method
is similar to {Concurrent::Promises::Future#flat} but it will keep flattening
until it's fulfilled with non future value, then the value is taken as a result
of the process simulated by `run`.

```ruby
body = lambda do |v|
  # Some computation step of the process    
  new_v = v + 1
  # Is the process finished?
  if new_v < 5
    # Continue computing with new value, does not have to be recursive.
    # It just has to return a future.
    Concurrent::Promises.future(new_v, &body)
  else
    # The process is finished, fulfill the final value with `new_v`.
    new_v
  end
end
Concurrent::Promises.future(0, &body).run.value! # => 5
```

This solution works well an any Ruby implementation.

> *TODO: More examples to be added.*

## Throttling concurrency

By creating an actor managing the resource we can control how many threads is
accessing the resource. In this case one at the time.

```ruby
data      = Array.new(10) { |i| '*' * i }
DB = Concurrent::Actor::Utils::AdHoc.spawn :db, data do |data|
  lambda do |message|
    # pretending that this queries a DB
    data[message]
  end
end

concurrent_jobs = 11.times.map do |v|
  DB.
      # ask the DB with the `v`, only one at the time, rest is parallel
      ask(v).
      # get size of the string, rejects for 11
      then(&:size).
      # translate error to a value (message of the exception)
      rescue { |reason| reason.message } 
end #

Concurrent::Promises.zip(*concurrent_jobs).value!
```

Often there is more then one DB connections, then the pool can be used.

```ruby
pool_size = 5

DB_POOL = Concurrent::Actor::Utils::Pool.spawn!('DB-pool', pool_size) do |index|
  # DB connection constructor
  Concurrent::Actor::Utils::AdHoc.spawn(
      name: "connection-#{index}", 
      args: [data]) do |data|
    lambda do |message|
      # pretending that this queries a DB
      data[message]
    end
  end
end

concurrent_jobs = 11.times.map do |v|
  DB_POOL.
      # ask the DB with the `v`, only one at the time, rest is parallel
      ask(v).
      # get size of the string, rejects for 11
      then(&:size).
      # translate error to a value (message of the exception)
      rescue { |reason| reason.message } 
end #

Concurrent::Promises.zip(*concurrent_jobs).value!
```

In other cases the DB adapter maintains its internal connection pool and we
just need to limit concurrent access to the DB's API to avoid the calls being
blocked.

Lets pretend that the `#[]` method on `DB_INTERNAL_POOL` is using the internal
pool of size 3. We create throttle with the same size

```ruby
DB_INTERNAL_POOL = Concurrent::Array.new data 

max_tree = Concurrent::Throttle.new 3

futures = 11.times.map do |i|
  max_tree.
      # throttled tasks, at most 3 simultaneous calls of [] on the database
      future { DB_INTERNAL_POOL[i] }.
      # un-throttled tasks, unlimited concurrency
      then { |starts| starts.size }.
      rescue { |reason| reason.message }
end #

futures.map(&:value!)
```

## Long stream of tasks, applying backpressure

Let's assume that we are querying an API for data and the queries can be faster
than we are able to process them. This example shows how to use channel as a
buffer and how to apply backpressure to slow down the queries. 

```ruby
require 'json' #

channel              = Concurrent::Promises::Channel.new 6
cancellation, origin = Concurrent::Cancellation.new

def query_random_text(cancellation, channel)
  Concurrent::Promises.future do
    # for simplicity the query is omitted
    # url = 'some api'
    # Net::HTTP.get(URI(url))
    sleep 0.01
    { 'message' => 
        'Lorem ipsum rhoncus scelerisque vulputate diam inceptos' 
    }.to_json
  end.then_flat_event(cancellation) do |value, cancellation|
    # The push to channel is fulfilled only after the message is successfully
    # published to the channel, therefore it will not continue querying until 
    # current message is pushed.
    cancellation.origin | channel.push_op(value) 
    # It could wait on the push indefinitely if the token is not checked
    # here with `or` (the pipe).        
  end.then(cancellation) do |cancellation|
    # query again after the message is pushed to buffer
    query_random_text(cancellation, channel) unless cancellation.canceled?
  end
end

words          = []
words_throttle = Concurrent::Throttle.new 1

def count_words_in_random_text(cancellation, channel, words, words_throttle)
  channel.pop_op.then do |response|
    string = JSON.load(response)['message']
    # processing is slower than querying
    sleep 0.02
    words_count = string.scan(/\w+/).size
  end.then_on(words_throttle.on(:io), words) do |words_count, words|
    # safe since throttled to only 1 task at a time
    words << words_count
  end.then_on(:io, cancellation) do |_, cancellation|
    # count words in next message
    unless cancellation.canceled?
      count_words_in_random_text(cancellation, channel, words, words_throttle)
    end
  end
end

query_processes = 3.times.map do
  Concurrent::Promises.future(cancellation, channel, &method(:query_random_text)).run
end

word_counter_processes = 2.times.map do
  Concurrent::Promises.future(cancellation, channel, words, words_throttle, 
      &method(:count_words_in_random_text)).run
end

sleep 0.05 #
```

Let it run for a while, then cancel it, and ensure that the runs were all fulfilled
(therefore ended) after the cancellation. Finally, print the result.

```ruby
origin.resolve
query_processes.map(&:wait!) 
word_counter_processes.map(&:wait!)
words
```

Compared to using threads directly, this is highly configurable and composable
solution.


## Periodic task

A periodically executed task can be creating by combining `schedule`, `run` and `Cancellation`.

```ruby
repeating_scheduled_task = -> interval, cancellation, task do
  Concurrent::Promises.
      # Schedule the task.
      schedule(interval, cancellation, &task).
      # If successful schedule again. 
      # Alternatively use chain to schedule always.
      then { repeating_scheduled_task.call(interval, cancellation, task) }
end

cancellation, origin = Concurrent::Cancellation.new

task = -> cancellation do
  5.times do
    cancellation.check!
    do_stuff
  end
end

result = Concurrent::Promises.future(0.1, cancellation, task, &repeating_scheduled_task).run
sleep 0.03 #
origin.resolve
result.result
```

