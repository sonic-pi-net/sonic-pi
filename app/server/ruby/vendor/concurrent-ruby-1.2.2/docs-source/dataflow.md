Data dependencies are `Future` values. The dataflow task itself is also a `Future` value, so you can build up a graph of these tasks, each of which is run when all the data and other tasks it depends on are available or completed. 

Our syntax is somewhat related to that of Akka's `flow` and Habanero Java's `DataDrivenFuture`. However unlike Akka we don't schedule a task at all until it is ready to run, and unlike Habanero Java we pass the data values into the task instead of dereferencing them again in the task. 

The theory of dataflow goes back to the 70s. In the terminology of the literature, our implementation is coarse-grained, in that each task can be many instructions, and dynamic in that you can create more tasks within other tasks. 

### Example

A dataflow task is created with the `dataflow` method, passing in a block.

```ruby
task = Concurrent::dataflow { 14 }
```

This produces a simple `Future` value. The task will run immediately, as it has no dependencies. We can also specify `Future` values that must be available before a task will run. When we do this we get the value of those futures passed to our block. 

```ruby
a = Concurrent::dataflow { 1 }
b = Concurrent::dataflow { 2 }
c = Concurrent::dataflow(a, b) { |av, bv| av + bv }
```

Using the `dataflow` method you can build up a directed acyclic graph (DAG) of tasks that depend on each other, and have the tasks run as soon as their dependencies are ready and there is CPU capacity to schedule them. This can help you create a program that uses more of the CPU resources available to you. 

### Derivation

This section describes how we could derive dataflow from other primitives in this library.

Consider a naive fibonacci calculator.

```ruby
def fib(n)
  if n < 2
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

puts fib(14) #=> 377
```

We could modify this to use futures.

```ruby
def fib(n)
  if n < 2
    Concurrent::Future.new { n }
  else
    n1 = fib(n - 1).execute
    n2 = fib(n - 2).execute
    Concurrent::Future.new { n1.value + n2.value }
  end
end

f = fib(14) #=> #<Concurrent::Future:0x000001019ef5a0 ...
f.execute   #=> #<Concurrent::Future:0x000001019ef5a0 ...

sleep(0.5)

puts f.value #=> 377
```

One of the drawbacks of this approach is that all the futures start, and then most of them immediately block on their dependencies. We know that there's no point executing those futures until their dependencies are ready, so let's not execute each future until all their dependencies are ready. 

To do this we'll create an object that counts the number of times it observes a future finishing before it does something - and for us that something will be to execute the next future. 

```ruby
class CountingObserver

  def initialize(count, &block)
    @count = count
    @block = block
  end

  def update(time, value, reason)
    @count -= 1

    if @count <= 0
      @block.call()
    end
  end

end

def fib(n)
  if n < 2
    Concurrent::Future.new { n }.execute
  else
    n1 = fib(n - 1)
    n2 = fib(n - 2)

    result = Concurrent::Future.new { n1.value + n2.value }

    barrier = CountingObserver.new(2) { result.execute }
    n1.add_observer barrier
    n2.add_observer barrier

    n1.execute
    n2.execute

    result
  end
end
```

We can wrap this up in a dataflow utility.

```ruby
f = fib(14) #=> #<Concurrent::Future:0x00000101fca308 ...
sleep(0.5)

puts f.value #=> 377

def dataflow(*inputs, &block)
  result = Concurrent::Future.new(&block)

  if inputs.empty?
    result.execute
  else
    barrier = CountingObserver.new(inputs.size) { result.execute }

    inputs.each do |input|
      input.add_observer barrier
    end
  end

  result
end

def fib(n)
  if n < 2
    dataflow { n }
  else
    n1 = fib(n - 1)
    n2 = fib(n - 2)
    dataflow(n1, n2) { n1.value + n2.value }
  end
end

f = fib(14) #=> #<Concurrent::Future:0x00000101fca308 ...
sleep(0.5)

puts f.value #=> 377
```

Since we know that the futures the dataflow computation depends on are already going to be available when the future is executed, we might as well pass the values into the block so we don't have to reference the futures inside the block. This allows us to write the dataflow block as straight non-concurrent code without reference to futures. 

```ruby
def dataflow(*inputs, &block)
  result = Concurrent::Future.new do
    values = inputs.map { |input| input.value }
    block.call(*values)
  end

  if inputs.empty?
    result.execute
  else
    barrier = CountingObserver.new(inputs.size) { result.execute }

    inputs.each do |input|
      input.add_observer barrier
    end
  end

  result
end

def fib(n)
  if n < 2
    Concurrent::dataflow { n }
  else
    n1 = fib(n - 1)
    n2 = fib(n - 2)
    Concurrent::dataflow(n1, n2) { |v1, v2| v1 + v2 }
  end
end

f = fib(14) #=> #<Concurrent::Future:0x000001019a26d8 ...
sleep(0.5)

puts f.value #=> 377
```
