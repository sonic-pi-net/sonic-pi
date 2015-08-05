`TVar` and `atomically` implement a software transactional memory. A `TVar` is a
single item container that always contains exactly one value. The `atomically`
method allows you to modify a set of `TVar` objects with the guarantee that all
of the updates are collectively atomic - they either all happen or none of them
do - consistent - a `TVar` will never enter an illegal state - and isolated -
atomic blocks never interfere with each other when they are running. You may
recognise these properties from database transactions.

There are some very important and unusual semantics that you must be aware of:

*   Most importantly, the block that you pass to `atomically` may be executed more
than once. In most cases your code should be free of side-effects, except for
via `TVar`.

*   If an exception escapes an `atomically` block it will abort the transaction.

*   It is undefined behaviour to use `callcc` or `Fiber` with `atomically`.

*   If you create a new thread within an `atomically`, it will not be part of
the transaction. Creating a thread counts as a side-effect.

We implement nested transactions by flattening.

We only support strong isolation if you use the API correctly. In order words,
we do not support strong isolation.

Our implementation uses a very simple two-phased locking with versioned locks
algorithm and lazy writes, as per [1]. In the future we will look at more
advanced algorithms, contention management and using existing Java
implementations when in JRuby.

See:

1.  T. Harris, J. Larus, and R. Rajwar. Transactional Memory. Morgan & Claypool, second edition, 2010.

## Motivation

Consider an application that transfers money between bank accounts. We want to
transfer money from one account to another. It is very important that we don't
lose any money! But it is also important that we can handle many account
transfers at the same time, so we run them concurrently, and probably also in
parallel.

This code shows us transferring ten pounds from one account to another.

```ruby
a = BankAccount.new(100_000)
b = BankAccount.new(100)

a.value -= 10
b.value += 10
```

Before we even start to talk about to talk about concurrency and parallelism, is
this code safe? What happens if after removing money from account a, we get an
exception? It's a slightly contrived example, but if the account totals were
very large, adding to them could involve the stack allocation of a `BigNum`, and
so could cause out of memory exceptions.  In that case the money would have
disappeared from account a, but not appeared in account b. Disaster!

So what do we really need to do?

```ruby
a = BankAccount.new(100_000)
b = BankAccount.new(100)

original_a = a.value
a.value -= 10

begin
  b.value += 10
rescue e =>
  a.value = original_a
  raise e
end
```

This rescues any exceptions raised when setting b and will roll back the change
we have already made to b. We'll keep this rescue code in mind, but we'll leave
it out of future examples for simplicity.

That might have made the code work when it only runs sequentially. Lets start to
consider some concurrency. It's obvious that we want to make the transfer of
money mutually exclusive with any other transfers - in order words it is a
critical section.

The usual solution to this would be to use a lock.

```ruby
lock.synchronize do
  a.value -= 10
  b.value += 10
end
```

That should work. Except we said we'd like these transfer to run concurrently,
and in parallel. With a single lock like that we'll only let one transfer take
place at a time. Perhaps we need more locks? We could have one per account:

```ruby
a.lock.synchronize do
  b.lock.synchronize do
    a.value -= 10
    b.value += 10
  end
end
```

However this is vulnerable to deadlock. If we tried to transfer from a to b, at
the same time as from b to a, it's possible that the first transfer locks a, the
second transfer locks b, and then they both sit there waiting forever to get the
other lock. Perhaps we can solve that by applying a total ordering to the locks
and always acquire them in the same order?

```ruby
locks_needed = [a.lock, b.lock]
locks_in_order = locks_needed.sort{ |x, y| x.number <=> y.number }

locks_in_order[0].synchronize do
  locks_in_order[1].synchronize do
    a.value -= 10
    b.value += 10
  end
end
```

That might work. But we need to know exactly what locks we're going to need
before we start. If there were conditions in side the transfer this might be
more complicated. We also need to remember the rescue code we had above to deal
with exceptions. This is getting out of hand - and it's where `TVar` comes in.

We'll model the accounts as `TVar` - transactional variable, and instead of
locks we'll use `Concurrent::atomically`.

```ruby
a = TVar.new(100_000)
b = TVar.new(100)

Concurrent::atomically do
  a.value -= 10
  b.value += 10
end
```

That short piece of code effectively solves all the concerns we identified
above. How it does it is described in the reference above. You just need to be
happy that any two `atomically` blocks (we call them transactions) that use an
overlapping set of `TVar` objects will appear to have happened as if there was a
big global lock on them, and that if any exception is raised in the block, it
will be as if the block never happened. But also keep in mind the important
points we detailed right at the start of the article about side effects and
repeated execution.

## Evaluation

We evaluated the performance of our `TVar` implementation using a bank account
simulation with a range of synchronisation implementations. The simulation
maintains a set of bank account totals, and runs transactions that either get a
summary statement of multiple accounts (a read-only operation) or transfers a
sum from one account to another (a read-write operation).

We implemented a bank that does not use any synchronisation (and so creates
inconsistent totals in accounts), one that uses a single global (or 'coarse')
lock (and so won't scale at all), one that uses one lock per account (and so has
a complicated system for locking in the correct order) and one using our `TVar`
and `atomically`.

We ran 1 million transactions divided equally between a varying number of
threads on a system that has at least that many physical cores. The transactions
are made up of a varying mixture of read-only and read-write transactions. We
ran each set of transactions thirty times, discarding the first ten and then
taking an algebraic mean. These graphs show only the simple mean. Our `tvars-
experiments` branch includes the benchmark used, full details of the test
system, and all the raw data.

Using JRuby using 75% read-write transactions, we can compare how the different
implementations of bank accounts scales to more cores. That is, how much faster
it runs if you use more cores.

![](https://raw.githubusercontent.com/ruby-concurrency/concurrent-ruby/master/doc/images/tvar/implementation-scalability.png)

We see that the coarse lock implementation does not scale at all, and in fact
with more cores only wastes more time in contention for the single global lock.
We see that the unsynchronised implementation doesn't seem to scale well - which
is strange as there should be no overhead, but we'll explain that in a second.
We see that the fine lock implementation seems to scale better, and that the
`TVar` implementation scales the best.

So the `TVar` implementation *scales* very well, but how absolutely fast is it?

![](https://raw.githubusercontent.com/ruby-concurrency/concurrent-ruby/master/doc/images/tvar/implementation-absolute.png)

Well, that's the downside. The unsynchronised implementation doesn't scale well
because it's so fast in the first place, and probably because we're bound on
access to the memory - the threads don't have much work to do, so no matter how
many threads we have the system is almost always reaching out to the L3 cache or
main memory. However remember that the unsynchronised implementation isn't
correct - the totals are wrong at the end. The coarse lock implementation has an
overhead of locking and unlocking. The fine lock implementation has a greater
overhead as as the locking scheme is complicated to avoid deadlock. It scales
better, however, actually allowing transactions to be processed in parallel. The
`TVar` implementation has a greater overhead still - and it's pretty huge. That
overhead is the cost for the simple programming model of an atomic block.

So that's what `TVar` gives you at the moment - great scalability, but it has a
high overhead. That's pretty much the state of software transactional memory in
general. Perhaps hardware transactional memory will help us, or perhaps we're
happy anyway with the simpler and safer programming model that the `TVar` gives
us.

We can also use this experiment to compare different implementations of Ruby. We
looked at just the `TVar` implementation and compared MRI 2.1.1, Rubinius 2.2.6,
and JRuby 1.7.11, again at 75% write transactions.

![](https://raw.githubusercontent.com/ruby-concurrency/concurrent-ruby/master/doc/images/tvar/ruby-scalability.png)

We see that MRI provides no scalability, due to the global interpreter lock
(GIL). JRuby seems to scale better than Rubinius for this workload (there are of
course other workloads).

As before we should also look at the absolute performance, not just the
scalability.

![](https://raw.githubusercontent.com/ruby-concurrency/concurrent-ruby/master/doc/images/tvar/ruby-absolute.png)

Again, JRuby seems to be faster than Rubinius for this experiment.
Interestingly, Rubinius looks slower than MRI for 1 core, but we can get around
that by using more cores.

We've used 75% read-write transactions throughout. We'll just take a quick look
at how the scalability varies for different workloads, for scaling between 1 and
2 threads. We'll admit that we used 75% read-write just because it emphasised
the differences.

![](https://raw.githubusercontent.com/ruby-concurrency/concurrent-ruby/master/doc/images/tvar/implementation-write-proportion-scalability.png)

Finally, we can also run on a larger machine. We repeated the experiment using a
machine with 64 physical cores and JRuby.

![](https://raw.githubusercontent.com/ruby-concurrency/concurrent-ruby/master/doc/images/tvar/implementation-scalability.png)

![](https://raw.githubusercontent.com/ruby-concurrency/concurrent-ruby/master/doc/images/tvar/implementation-absolute.png)

Here you can see that `TVar` does become absolutely faster than using a global
lock, at the slightly ridiculously thread-count of 50. It's probably not
statistically significant anyway.
