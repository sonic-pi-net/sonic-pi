### Simple asynchronous task

future = Concurrent.future { sleep 0.1; 1 + 1 } # evaluation starts immediately
future.completed?
# block until evaluated
future.value
future.completed?


### Failing asynchronous task

future = Concurrent.future { raise 'Boom' }
future.value
future.value! rescue $!
future.reason
# re-raising
raise future rescue $!


### Chaining

head    = Concurrent.succeeded_future 1 #
branch1 = head.then(&:succ) #
branch2 = head.then(&:succ).then(&:succ) #
branch1.zip(branch2).value!
(branch1 & branch2).then { |a, b| a + b }.value!
(branch1 & branch2).then(&:+).value!
Concurrent.zip(branch1, branch2, branch1).then { |*values| values.reduce &:+ }.value!
# pick only first completed
(branch1 | branch2).value!

### Error handling

Concurrent.future { Object.new }.then(&:succ).then(&:succ).rescue { |e| e.class }.value # error propagates
Concurrent.future { Object.new }.then(&:succ).rescue { 1 }.then(&:succ).value
Concurrent.future { 1 }.then(&:succ).rescue { |e| e.message }.then(&:succ).value

failing_zip = Concurrent.succeeded_future(1) & Concurrent.failed_future(StandardError.new('boom'))
failing_zip.result
failing_zip.then { |v| 'never happens' }.result
failing_zip.rescue { |a, b| (a || b).message }.value
failing_zip.chain { |success, values, reasons| [success, values.compact, reasons.compactß] }.value

### Delay

# will not evaluate until asked by #value or other method requiring completion
future = Concurrent.delay { 'lazy' }
sleep 0.1 #
future.completed?
future.value

# propagates trough chain allowing whole or partial lazy chains

head    = Concurrent.delay { 1 }
branch1 = head.then(&:succ)
branch2 = head.delay.then(&:succ)
join    = branch1 & branch2

sleep 0.1 # nothing will complete
[head, branch1, branch2, join].map(&:completed?)

branch1.value
sleep 0.1 # forces only head to complete, branch 2 stays incomplete
[head, branch1, branch2, join].map(&:completed?)

join.value


### Flatting

Concurrent.future { Concurrent.future { 1+1 } }.flat.value # waits for inner future

# more complicated example
Concurrent.future { Concurrent.future { Concurrent.future { 1 + 1 } } }.
    flat(1).
    then { |f| f.then(&:succ) }.
    flat(1).value


### Schedule

scheduled = Concurrent.schedule(0.1) { 1 }

scheduled.completed?
scheduled.value # available after 0.1sec

# and in chain
scheduled = Concurrent.delay { 1 }.schedule(0.1).then(&:succ)
# will not be scheduled until value is requested
sleep 0.1 #
scheduled.value # returns after another 0.1sec


### Completable Future and Event

future = Concurrent.future
event  = Concurrent.event
# Don't forget to keep the reference, `Concurrent.future.then { |v| v }` is incompletable

# will be blocked until completed
t1     = Thread.new { future.value } #
t2     = Thread.new { event.wait } #

future.success 1
future.success 1 rescue $!
future.try_success 2
event.complete

[t1, t2].each &:join #


### Callbacks

queue  = Queue.new
future = Concurrent.delay { 1 + 1 }

future.on_success { queue << 1 } # evaluated asynchronously
future.on_success! { queue << 2 } # evaluated on completing thread

queue.empty?
future.value
queue.pop
queue.pop


### Thread-pools

Concurrent.future(:fast) { 2 }.then(:io) { File.read __FILE__ }.wait


### Interoperability with actors

actor = Concurrent::Actor::Utils::AdHoc.spawn :square do
  -> v { v ** 2 }
end

Concurrent.
    future { 2 }.
    then_ask(actor).
    then { |v| v + 2 }.
    value

actor.ask(2).then(&:succ).value


### Interoperability with channels

ch1 = Concurrent::Edge::Channel.new
ch2 = Concurrent::Edge::Channel.new

result = Concurrent.select(ch1, ch2)
ch1.push 1
result.value!

Concurrent.
    future { 1+1 }.
    then_push(ch1)
result = Concurrent.
    future { '%02d' }.
    then_select(ch1, ch2).
    then { |format, (value, channel)| format format, value }
result.value!


### Common use-cases Examples

# simple background processing
Concurrent.future { do_stuff }

# parallel background processing
jobs = 10.times.map { |i| Concurrent.future { i } } #
Concurrent.zip(*jobs).value


# periodic task
@end = false

def schedule_job
  Concurrent.schedule(1) { do_stuff }.
      rescue { |e| StandardError === e ? report_error(e) : raise(e) }.
      then { schedule_job unless @end }
end

schedule_job
@end = true


# How to limit processing where there are limited resources?
# By creating an actor managing the resource
DB   = Concurrent::Actor::Utils::AdHoc.spawn :db do
  data = Array.new(10) { |i| '*' * i }
  lambda do |message|
    # pretending that this queries a DB
    data[message]
  end
end

concurrent_jobs = 11.times.map do |v|
  Concurrent.
      future { v }.
      # ask the DB with the `v`, only one at the time, rest is parallel
      then_ask(DB).
      # get size of the string, fails for 11
      then(&:size).
      rescue { |reason| reason.message } # translate error to value (exception, message)
end #

Concurrent.zip(*concurrent_jobs).value!


# In reality there is often a pool though:
data      = Array.new(10) { |i| '*' * i }
pool_size = 5

DB_POOL = Concurrent::Actor::Utils::Pool.spawn!('DB-pool', pool_size) do |index|
  # DB connection constructor
  Concurrent::Actor::Utils::AdHoc.spawn(name: "worker-#{index}", args: [data]) do |data|
    lambda do |message|
      # pretending that this queries a DB
      data[message]
    end
  end
end

concurrent_jobs = 11.times.map do |v|
  Concurrent.
      future { v }.
      # ask the DB_POOL with the `v`, only 5 at the time, rest is parallel
      then_ask(DB_POOL).
      then(&:size).
      rescue { |reason| reason.message }
end #

Concurrent.zip(*concurrent_jobs).value!
