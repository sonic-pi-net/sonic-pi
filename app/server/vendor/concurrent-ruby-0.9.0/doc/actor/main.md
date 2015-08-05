# Actor model

-  Light-weighted running on thread-pool.
-  Inspired by Akka and Erlang.
-  Modular.

This Actor model implementation makes makes actors very cheap to create and discard.
Thousands of actors can be created, allowing you to break the program into smaller 
maintainable pieces, without violating the single responsibility principle.

## What is an actor model?

Actor-based concurrency is all the rage in some circles. Originally described in 1973, the actor model is a paradigm 
for creating asynchronous, concurrent objects  that is becoming increasingly popular. Much has changed since actors 
were first  written about four decades ago, which has led to a serious fragmentation within  the actor community. 
There is *no* universally accepted, strict definition of "actor" and actor implementations differ widely between 
languages and libraries.  

[Wiki](http://en.wikipedia.org/wiki/Actor_model) definition is pretty good:
_The actor model in computer science is a mathematical model of concurrent computation
that treats **actors** as the universal primitives of concurrent digital computation:
in response to a message that it receives, an actor can make local decisions,
create more actors, send more messages, and determine how to respond to the next
message received._

## Why?

Concurrency is hard to get right, actors are one of many ways how to simplify the problem.

## Quick example

An example:

```ruby
class Counter < Concurrent::Actor::Context
  # Include context of an actor which gives this class access to reference
  # and other information about the actor

  # use initialize as you wish
  def initialize(initial_value)
    @count = initial_value
  end

  # override on_message to define actor's behaviour
  def on_message(message)
    if Integer === message
      @count += message
    end
  end
end #

# Create new actor naming the instance 'first'.
# Return value is a reference to the actor, the actual actor is never returned.
counter = Counter.spawn(:first, 5)

# Tell a message and forget returning self.
counter.tell(1)
counter << 1
# (First counter now contains 7.)

# Send a messages asking for a result.
counter.ask(0).class
counter.ask(0).value
```

{include:file:doc/actor/quick.out.rb}

## Spawning actors

-   {Concurrent::Actor.spawn} and {Concurrent::Actor.spawn!}
-   {Concurrent::Actor::AbstractContext.spawn} and {Concurrent::Actor::AbstractContext.spawn!} 

## Sending messages

-   {Concurrent::Actor::Reference#tell} 
    {include:Concurrent::Actor::Reference#tell}
-   {Concurrent::Actor::Reference#ask}
    {include:Concurrent::Actor::Reference#ask}
-   {Concurrent::Actor::Reference#ask!}
    {include:Concurrent::Actor::Reference#ask!}

Messages are processed in same order as they are sent by a sender. It may interleaved with
messages from other senders though.

### Immutability

Messages sent between actors should be **immutable**. Gems like

- [Algebrick](https://github.com/pitr-ch/algebrick) - Typed struct on steroids based on
  algebraic types and pattern matching
- [Hamster](https://github.com/hamstergem/hamster) - Efficient, Immutable, Thread-Safe
  Collection classes for Ruby

are very helpful.

{include:file:doc/actor/messaging.out.rb}

## Actor definition

{include:Concurrent::Actor::AbstractContext}

## Reference

{include:Actor::Reference}

## Garbage collection

Spawned actor cannot be garbage-collected until it's terminated. There is a reference held in the parent actor.

## Parent-child relationship, name, and path

-   {Core#name} 
    {include:Actor::Core#name}
-   {Core#path}
    {include:Actor::Core#path}
-   {Core#parent}
    {include:Actor::Core#parent} 

## Behaviour

{include:Actor::Behaviour}

## IO cooperation

Actors are running on shared thread poll which allows user to create many actors cheaply.
Downside is that these actors cannot be directly used to do IO or other blocking operations.
Blocking operations could starve the `default_task_pool`. However there are two options:

- Create an regular actor which will schedule blocking operations in `global_operation_pool`
  (which is intended for blocking operations) sending results back to self in messages.
- Create an actor using `global_operation_pool` instead of `global_task_pool`, e.g.
  `AnIOActor.spawn name: :blocking, executor: Concurrent.configuration.global_operation_pool`.
  
### Example
  
{include:file:doc/actor/io.out.rb}

## Dead letter routing

see {AbstractContext#dead_letter_routing} description:

> {include:Actor::AbstractContext#dead_letter_routing}

## FAQ

###   What happens if I try to supervise using a normal Context?

Alleged supervisor will receive errors from its supervised actors. They'll have to be handled manually.

### How to change supervision strategy?

Use option `behaviour_definition: Behaviour.restarting_behaviour_definition(:resume!)` or 
`behaviour_definition: Behaviour.restarting_behaviour_definition(:reset!, :one_for_all)`

### How to change behaviors?

Any existing behavior can be subclassed 

### How to implement custom restarting?

By subclassing {Behaviour::Pausing} and overriding {Behaviour::Pausing#restart!}. Implementing 
{AbstractContext#on_event} could be also considered.

_We'll be happy to answer any other questions, 
just [open an Issue](https://github.com/ruby-concurrency/concurrent-ruby/issues/new) or find us on 
https://gitter.im/ruby-concurrency/concurrent-ruby._

## Speed

Simple benchmark Actor vs Celluloid, the numbers are looking good
but you know how it is with benchmarks. Source code is in
`examples/actor/celluloid_benchmark.rb`. It sends numbers between x actors
and adding 1 until certain limit is reached.

Benchmark legend:

- mes.  - number of messages send between the actors
- act.  - number of actors exchanging the messages
- impl. - which gem is used

### JRUBY

    Rehearsal ---------------------------------------------------------
    50000    2 concurrent  26.140000   0.610000  26.750000 (  7.761000)
    50000    2 celluloid   41.440000   5.270000  46.710000 ( 17.535000)
    50000  500 concurrent  11.340000   0.180000  11.520000 (  3.498000)
    50000  500 celluloid   19.310000  10.680000  29.990000 ( 14.619000)
    50000 1000 concurrent  10.640000   0.180000  10.820000 (  3.563000)
    50000 1000 celluloid   17.840000  19.850000  37.690000 ( 18.892000)
    50000 1500 concurrent  14.120000   0.290000  14.410000 (  4.618000)
    50000 1500 celluloid   19.060000  28.920000  47.980000 ( 25.185000)
    ---------------------------------------------- total: 225.870000sec
    
     mes. act.      impl.       user     system      total        real
    50000    2 concurrent   7.320000   0.530000   7.850000 (  3.637000)
    50000    2 celluloid   13.780000   4.730000  18.510000 ( 10.756000)
    50000  500 concurrent   9.270000   0.140000   9.410000 (  3.020000)
    50000  500 celluloid   16.540000  10.920000  27.460000 ( 14.308000)
    50000 1000 concurrent   9.970000   0.160000  10.130000 (  3.445000)
    50000 1000 celluloid   15.930000  20.840000  36.770000 ( 18.272000)
    50000 1500 concurrent  11.580000   0.240000  11.820000 (  3.723000)
    50000 1500 celluloid   19.440000  29.060000  48.500000 ( 25.227000) (1)

### MRI 2.1.0

    Rehearsal ---------------------------------------------------------
    50000    2 concurrent   4.180000   0.080000   4.260000 (  4.269435)
    50000    2 celluloid    7.740000   3.100000  10.840000 ( 10.043875)
    50000  500 concurrent   5.900000   1.310000   7.210000 (  6.565067)
    50000  500 celluloid   12.820000   5.810000  18.630000 ( 17.320765)
    50000 1000 concurrent   6.080000   1.640000   7.720000 (  6.931294)
    50000 1000 celluloid   17.130000   8.320000  25.450000 ( 23.786146)
    50000 1500 concurrent   6.940000   2.030000   8.970000 (  7.927330)
    50000 1500 celluloid   20.980000  12.040000  33.020000 ( 30.849578)
    ---------------------------------------------- total: 116.100000sec
    
     mes. act.      impl.       user     system      total        real
    50000    2 concurrent   3.730000   0.100000   3.830000 (  3.822688)
    50000    2 celluloid    7.900000   2.910000  10.810000 (  9.924014)
    50000  500 concurrent   5.420000   1.230000   6.650000 (  6.025579)
    50000  500 celluloid   12.720000   5.540000  18.260000 ( 16.889517)
    50000 1000 concurrent   5.420000   0.910000   6.330000 (  5.896689)
    50000 1000 celluloid   16.090000   8.040000  24.130000 ( 22.347102)
    50000 1500 concurrent   5.580000   0.760000   6.340000 (  6.038535)
    50000 1500 celluloid   20.000000  11.680000  31.680000 ( 29.590774) (1)

*Note (1):* Celluloid is using thread per actor so this bench is creating about 1500
native threads. Actor is using constant number of threads.
