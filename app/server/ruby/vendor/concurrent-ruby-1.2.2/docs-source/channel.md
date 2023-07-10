# Channels and Goroutines

Channels, popularized by the [Go programming language](https://golang.org/doc/effective_go.html#channels), are a modern variation of [communicating sequential processes (CSP)](https://en.wikipedia.org/wiki/Communicating_sequential_processes). CSP was first proposed by  C. A. R. Hoare in 1978. The Go philosophy on concurrency is:

> Do not communicate by sharing memory; instead, share memory by communicating.

As [Rob Pike](https://en.wikipedia.org/wiki/Rob_Pike) eloquently explains in his [Concurrency Is Not Parallelism](https://vimeo.com/49718712) conference talk, concurrency is the "composition of independently executing things." Combining these two ideas, channels are a queue-like mechanism that can be used to communicate between independently executing things.

The channel implementation in this library was highly influenced by Go, but also incorporates ideas from Clojure's [core.async](https://clojure.github.io/core.async/) library. Runtime differences aside, this channel library is functionally equivalent to Go and even includes a few features Go does not.

### Example Programs

Every code example in the channel chapters of both [A Tour of Go](https://tour.golang.org/welcome/1) and [Go By Example](https://gobyexample.com/) has been reproduced in Ruby. The code can be found in the [examples](https://github.com/ruby-concurrency/concurrent-ruby/tree/master/examples) directory of the source repository. Many of those examples appear in the documentation below, but many do not. They are a valuable resource for learning how to use channels.

### Additional Resources

* "A Tour of Go" [channels exercises](https://tour.golang.org/concurrency/2)
* "Go By Example" [channels exercises](https://gobyexample.com/channels)
* "Effective Go" [concurrency chapters](https://golang.org/doc/effective_go.html#concurrency)
* "Concurrency Is Not Parallelims" [conference presentation](https://vimeo.com/49718712) by Rob Pike, principal designer of Go
* "Clojure core.async Channels" [blog post](http://clojure.com/blog/2013/06/28/clojure-core-async-channels.html) by Rich Hickey, inventor of Clojure
* Clojure core.async [API reference](https://clojure.github.io/core.async/)

## Goroutines

The Go programming language uses "goroutines" as the core concurrency mechanism. A goroutine is little more than an independently executing function, multiplexed with all other goroutines onto a thread pool managed by the runtime. Ruby has a very different runtime so true goroutines are not possible. Instead, a {.go} method is provided for running a block asynchronously, multiplexed onto a special thread pool reserved just for `Channel` operations. This is similar to what Clojure does with the `go` function from the [core.async](https://clojure.github.io/core.async/#clojure.core.async/go) library.

```ruby
puts "Main thread: #{Thread.current}"

Concurrent::Channel.go do
  puts "Goroutine thread: #{Thread.current}"
end

# Main thread: #<Thread:0x007fcb4c8bc3f0>
# Goroutine thread: #<Thread:0x007fcb4c21f4e8>
```

Although it is possible to use `Channel.go` independent of channels or to use channels with other asynchronous processing tools (such as `Future` and `Actor`), mixing the tools is not advised. Most high-level asynchronous processing tools already have a queue-like mechanism built in. Adding channels to the mix may indicate a design flaw. Conversely, `Channel.go` provides no mechanism for coordination and communication. That's what channels are for. Additionally, strictly using `Channel.go` along with channels provides an opportunity for future optimizations, such as Clojure's inversion of control (IOC) threads.

## Channel Basics

Channels are "pipes" through which values can be sent. They are thread safe and naturally concurrent. When shared between goroutines they provide a communication mechanism for coordinating asynchronous actions.

The core channel operations are {#put} and {#take} (aliased as {#send} and {#receive}, respectively). The former function inserts a value into a channel where the latter removes a value. By default these are blocking operations. A call to `put` will block until the channel is ready to receive the value. Similarly, a call to `take` will block until a value is available to be removed.

The following, simple example creates a channel, launches a goroutine from which a value is placed into the channel, then reads that value from the channel. When run this example will display "ping" in the console.

```ruby
messages = Concurrent::Channel.new

Concurrent::Channel.go do
  messages.put 'ping'
end

msg = messages.take
puts msg
```

By default, channels are *unbuffered*, meaning that they have a capacity of zero and only accept puts and takes when both a putting and a taking thread are available. If a `put` is started when there is no taker thread the call will block. As soon as another thread calls `take` the exchange will occur and both calls will return on their respective threads. Similarly, if a `take` is started when there is no putting thread the call will block until another thread calls `put`.

The following, slightly more complex example, concurrently sums two different halves of a list then combines the results. It uses an unbuffered channel to pass the results from the two goroutines back to the main thread. The main thread blocks on the two `take` calls until the worker goroutines are done. This example also uses the convenience aliases {#<<} and {#~}. Since channels in Go are part of the language, channel operations are performed using special channel operators rather than functions. These operators help clearly indicate that channel operations are being performed. The operator overloads `<<` for `put` and `~` for `take` help reinforce this idea in Ruby.

```ruby
def sum(a, c)
  sum = a.reduce(0, &:+)
  c << sum # `<<` is an alias for `put` or `send`
end

a = [7, 2, 8, -9, 4, 0]
l = a.length / 2
c = Concurrent::Channel.new

Concurrent::Channel.go { sum(a[-l, l], c) }
Concurrent::Channel.go { sum(a[0, l], c) }
x, y = ~c, ~c # `~` is an alias for `take` or `receive`

puts [x, y, x+y].join(' ')
```

## Channel Buffering

One common channel variation is a *buffered* channel. A buffered channel has a finite number of slots in the buffer which can be filled. Putting threads can put values into the channel even if there is no taking threads, up to the point where the buffer is filled. Once a buffer becomes full the normal blocking behavior resumes. A buffered channel is created by giving a `:capacity` option on channel creation:

The following example creates a buffered channel with two slots. It then makes two `put` calls, adding values to the channel. These calls do not block because the buffer has room. Were a third `put` call to be made before an `take` calls, the third `put` would block.

```ruby
ch = Concurrent::Channel.new(capacity: 2)
ch << 1
ch << 2

puts ~ch
puts ~ch
```

## Channel Synchronization

The main purpose of channels is to synchronize operations across goroutines. One common pattern for this is to create a `capacity: 1` buffered channel which is used to signal that work is complete. The following example calls a `worker` function on a goroutine and passes it a "done" channel. The main thread then calls `take` on the "done" channel and blocks until signaled.

```ruby
def worker(done_channel)
  print "working...\n"
  sleep(1)
  print "done\n"

  done_channel << true
end

done = Concurrent::Channel.new(capacity: 1)
Concurrent::Channel.go{ worker(done) }

~done # block until signaled
```

## Multichannel Select

Often it is necessary for a single thread to operate on more than one channel. The {.select} method facilitates multivariate channel operations. The `select` method takes a block and passes through a special "selector" object as the first block parameter. The selector can then be used to specify various channel operations. The `select` call will block until one of the operations occurs. If a block is provided for the triggered clause (required for some clauses, optional for others) the block will then be called. Finally, the `select` call will immediately exit, guaranteeing that only one of the select clauses will trigger.

The following example spawns two goroutines, each of which goes to sleep before putting a value onto a channel. The main thread loops twice over a `select` and, in each loop, takes a value off of whichever channel returns one first.

```ruby
c1 = Concurrent::Channel.new
c2 = Concurrent::Channel.new

Concurrent::Channel.go do
  sleep(1)
  c1 << 'one'
end

Concurrent::Channel.go do
  sleep(2)
  c1 << 'two'
end

2.times do
  Concurrent::Channel.select do |s|
    s.take(c1) { |msg| print "received #{msg}\n" }
    s.take(c2) { |msg| print "received #{msg}\n" }
  end
end
```

The output from the above example is:

```
received one
received two
```

The next example calculates the first 10 fibonacci numbers, passing them to the main thread via a channel. The fibonacci function puts each calculated value onto a channel while simultaneously listening to a different channel for the signal to stop. This example uses the `case` method on the selector object. This is just a convenience method for `put` and `take`, allowing the Ruby code to look more like Go.

```ruby
def fibonacci(c, quit)
  x, y = 0, 1
  loop do
    Concurrent::Channel.select do |s|
      s.case(c, :<<, x) { x, y = y, x+y; x } # alias for `s.put`
      s.case(quit, :~) do                    # alias for `s.take`
        puts 'quit'
        return
      end
    end
  end
end

c = Concurrent::Channel.new
quit = Concurrent::Channel.new

Concurrent::Channel.go do
  10.times { puts ~c }
  quit << 0
end

fibonacci(c, quit)
```

## Closing and Iterating Over Channels

Newly created channels are in an "open" state. Open channels can receive values via `put` operations. When a program is done with a channel it can be closed by calling the `#close` method. Once a channel is closed it will no longer allow values to be `put`. If the channel is buffered and values are in the buffer when the channel is closed, the remaining values can still be removed via `take` operations.

The `Channel` class implements an {#each} method which can be used to retrieve successive values from the channel. The `each` method is a blocking method. When the channel is open and there are no values in the buffer, `each` will block until a new item is `put`. The `each` method will not exit until the channel is closed.

The following example launches a goroutine which calculates several fibonacci values and puts them into a channel. The main thread uses the `each` method to retrieve all the values successively and display them in the console. Once the fibonacci goroutine is done it closes the channel which subsequently causes the `each` iteration to end, unblocking the main thread.

```ruby
def fibonacci(n, c)
  x, y = 0, 1
  (1..n).each do
    c << x
    x, y = y, x+y
  end
  c.close
end

chan = Concurrent::Channel.new(capacity: 10)
Concurrent::Channel.go { fibonacci(chan.capacity, chan) }
chan.each { |i| puts i }
```

`Channel` also includes Ruby's [Enumerable](http://ruby-doc.org/core-2.2.3/Enumerable.html) mixin, allowing for a wide range of list comprehensions. Since the `Enumerable` methods iterate over the entire set of objects they can only complete once the channel is closed. Calling a method from `Enumerable` on an open channel will cause the method to block until the channel is closed.

## Timers and Tickers

A {.timer} is a specialized channel which triggers at a predefined time, specified as a number of seconds in the future. It is similar in concept to a {Concurrent::ScheduledTask} but operates as a channel and can fully participate in all channel operations.

The following code example creates two timers with different delay values. The first timer is allowed to expire (trigger) by having the main thread perform a `take` on it. When the timer expires it puts a {Concurrent::Channel::Tick} object into its buffer and closes. The second timer is listened to on a goroutine but the it never expires: the main thread stops (closes) the timer before it expires. Note that the goroutine in this example blocks forever and never exits. Since the timer is closed it never puts the `Tick` into its buffer.

```ruby
timer1 = Concurrent::Channel.timer(2)

~timer1
puts 'Timer 1 expired'

timer2 = Concurrent::Channel.timer(1)
Concurrent::Channel.go do
  ~timer2
  print "Timer 2 expired\n"
end

stop2 = timer2.stop # alias for `close`
print "Timer 2 stopped\n" if stop2
```

A {.ticker} is a specialized channel which triggers over and over again at a predefined interval, specified as a number of seconds between ticks. It is similar in concept to a {Concurrent::TimerTask} but operates as a channel and can fully participate in all channel operations.

The following example creates a ticker which triggers every half-second. A goroutine iterates over the ticker using the `each` method, printing the tick at every interval. When the main thread stops (closes) the ticker the `each` call ends and the goroutine exits.

```ruby
ticker = Concurrent::Channel.ticker(0.5)
Concurrent::Channel.go do
  ticker.each do |tick|
    print "Tick at #{tick}\n"
  end
end

sleep(1.6)
ticker.stop # alias for `close`
print "Ticker stopped\n"
```

## Default Selection

As with a Ruby `case` statement, a `Channel.select` statement will accept a `default` clause which will trigger if none of the other clauses trigger. Not surprisingly, the `default` clause must be the last clause in a `select` block.

```ruby
tick = Concurrent::Channel.tick(0.1)  # alias for `ticker`
boom = Concurrent::Channel.after(0.5) # alias for `timer`

loop do
  Concurrent::Channel.select do |s|
    s.take(tick) { print "tick.\n" }
    s.take(boom) do
      print "BOOM!\n"
      exit
    end
    s.default do
      print "    .\n"
      sleep(0.05)
    end
  end
end
```

The output of this code example is:

```
.
.
tick.
.
.
tick.
.
.
tick.
.
.
tick.
.
.
tick.
BOOM!
```
