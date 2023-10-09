require 'concurrent/edge/promises'
require 'concurrent/atomic/count_down_latch'

RSpec.describe 'Concurrent::Promises' do

  include Concurrent::Promises::FactoryMethods

  describe 'chain_resolvable' do
    it 'event' do
      b = resolvable_event
      a = resolvable_event.chain_resolvable(b)
      a.resolve
      expect(b).to be_resolved
    end

    it 'future' do
      b = resolvable_future
      a = resolvable_future.chain_resolvable(b)
      a.fulfill :val
      expect(b).to be_resolved
      expect(b.value).to eq :val
    end
  end

  describe '.future' do
    it 'executes' do
      future = future { 1 + 1 }
      expect(future.value!).to eq 2

      future = fulfilled_future(1).then { |v| v + 1 }
      expect(future.value!).to eq 2

      future = future(1, 2, &-> v { v })
      expect { future.value! }.to raise_error ArgumentError, /wrong number of arguments/

      future = fulfilled_future(1).then(2, &-> v { v })
      expect { future.value! }.to raise_error ArgumentError, /wrong number of arguments/
    end

    it 'executes with args' do
      future = future(1, 2, &:+)
      expect(future.value!).to eq 3

      future = fulfilled_future(1).then(1) { |v, a| v + 1 }
      expect(future.value!).to eq 2
    end
  end

  describe '.delay' do

    def behaves_as_delay(delay, value)
      expect(delay.resolved?).to eq false
      expect(delay.value!).to eq value
    end

    specify do
      behaves_as_delay delay { 1 + 1 }, 2
      behaves_as_delay fulfilled_future(1).delay.then { |v| v + 1 }, 2
      behaves_as_delay delay(1) { |a| a + 1 }, 2
      behaves_as_delay fulfilled_future(1).delay.then { |v| v + 1 }, 2
    end
  end

  describe '.schedule' do
    it 'scheduled execution' do
      start  = Time.now.to_f
      queue  = Queue.new
      future = schedule(0.1) { 1 + 1 }.then { |v| queue.push(v); queue.push(Time.now.to_f - start); queue }

      expect(future.value!).to eq queue
      expect(queue.pop).to eq 2
      expect(queue.pop).to be >= 0.09

      start  = Time.now.to_f
      queue  = Queue.new
      future = resolved_event.
          schedule(0.1).
          then { 1 }.
          then { |v| queue.push(v); queue.push(Time.now.to_f - start); queue }

      expect(future.value!).to eq queue
      expect(queue.pop).to eq 1
      expect(queue.pop).to be >= 0.09
    end

    it 'scheduled execution in graph' do
      start  = Time.now.to_f
      queue  = Queue.new
      future = future { sleep 0.1; 1 }.
          schedule(0.1).
          then { |v| v + 1 }.
          then { |v| queue.push(v); queue.push(Time.now.to_f - start); queue }

      expect(future.value!).to eq queue
      expect(queue.pop).to eq 2
      expect(queue.pop).to be >= 0.09

      scheduled = resolved_event.schedule(0.1)
      expect(scheduled.resolved?).to be_falsey
      scheduled.wait
      expect(scheduled.resolved?).to be_truthy
    end

  end

  describe '.event' do
    specify do
      resolvable_event = resolvable_event()
      one              = resolvable_event.chain(1) { |arg| arg }
      join             = zip(resolvable_event).chain { 1 }
      expect(one.resolved?).to be false
      resolvable_event.resolve
      expect(one.value!).to eq 1
      expect(join.wait.resolved?).to be true
    end
  end

  describe '.future without block' do
    specify do
      resolvable_future = resolvable_future()
      one               = resolvable_future.then(&:succ)
      join              = zip_futures(resolvable_future).then { |v| v }
      expect(one.resolved?).to be false
      resolvable_future.fulfill 0
      expect(one.value!).to eq 1
      expect(join.wait!.resolved?).to be true
      expect(join.value!).to eq 0
    end
  end

  describe '.any_resolved' do
    it 'continues on first result' do
      f1 = resolvable_future
      f2 = resolvable_future
      f3 = resolvable_future

      any1 = any_resolved_future(f1, f2)
      any2 = f2 | f3

      f1.fulfill 1
      f2.reject StandardError.new

      expect(any1.value!).to eq 1
      expect(any2.reason).to be_a_kind_of StandardError
    end
  end

  describe '.any_fulfilled' do
    it 'continues on first result' do
      f1 = resolvable_future
      f2 = resolvable_future

      any = any_fulfilled_future(f1, f2)

      f1.reject StandardError.new
      f2.fulfill :value

      expect(any.value!).to eq :value
    end

    it 'treats a resolved Event as a fulfilled Future' do
        any = any_fulfilled_future(
          resolved_event,
          fulfilled_future(:value),
        )

        expect(any.value!).to eq nil
    end

    it 'treats a pending Event as a pending Future' do
        any = any_fulfilled_future(
          resolvable_event,
          fulfilled_future(:value),
        )

        expect(any.value!).to eq :value
    end
  end

  describe '.zip' do
    it 'waits for all results' do
      a = future { 1 }
      b = future { 2 }
      c = future { 3 }

      z1 = a & b
      z2 = zip a, b, c
      z3 = zip a
      z4 = zip

      expect(z1.value!).to eq [1, 2]
      expect(z2.value!).to eq [1, 2, 3]
      expect(z3.value!).to eq [1]
      expect(z4.value!).to eq []

      q = Queue.new
      z1.then { |*args| q << args }
      # first is an array because it is zipping so 2 arguments
      expect(q.pop).to eq [1, 2]

      z1.then { |*args| args }.then { |*args| q << args }
      # after then it is again just one argument
      expect(q.pop).to eq [[1, 2]]

      fulfilled_future([1, 2]).then { |*args| q << args }
      expect(q.pop).to eq [[1, 2]]

      z1.then { |a1, b1, c1| q << [a1, b1, c1] }
      expect(q.pop).to eq [1, 2, nil]

      z2.then { |a1, b1, c1| q << [a1, b1, c1] }
      expect(q.pop).to eq [1, 2, 3]

      z3.then { |a1| q << a1 }
      expect(q.pop).to eq 1

      z3.then { |*as| q << as }
      expect(q.pop).to eq [1]

      z4.then { |a1| q << a1 }
      expect(q.pop).to eq nil

      z4.then { |*as| q << as }
      expect(q.pop).to eq []

      expect(z1.then { |a1, b1| a1 + b1 }.value!).to eq 3
      expect(z1.then { |a1, b1| a1 + b1 }.value!).to eq 3
      expect(z1.then(&:+).value!).to eq 3
      expect(z2.then { |a1, b1, c1| a1 + b1 + c1 }.value!).to eq 6

      expect(future { 1 }.delay).to be_a_kind_of Concurrent::Promises::Future
      expect(future { 1 }.delay.wait!).to be_resolved
      expect(resolvable_event.resolve.delay).to be_a_kind_of Concurrent::Promises::Event
      expect(resolvable_event.resolve.delay.wait).to be_resolved

      a = future { 1 }
      b = future { raise 'b' }
      c = future { raise 'c' }

      zip(a, b, c).chain { |*args| q << args }
      expect(q.pop.flatten.map(&:class)).to eq [FalseClass, 0.class, NilClass, NilClass, NilClass, RuntimeError, RuntimeError]
      zip(a, b, c).rescue { |*args| q << args }
      expect(q.pop.map(&:class)).to eq [NilClass, RuntimeError, RuntimeError]

      expect(zip.wait(0.1)).to eq true
    end

    context 'when a future raises an error' do

      let(:a_future) { future { raise 'error' } }

      it 'raises a concurrent error' do
        expect { zip(a_future).value! }.to raise_error(StandardError, 'error')
      end

      context 'when deeply nested' do
        it 'raises the original error' do
          expect { zip(zip(a_future)).value! }.to raise_error(StandardError, 'error')
        end
      end
    end
  end

  describe '.zip_events' do
    it 'waits for all and returns event' do
      a = fulfilled_future 1
      b = rejected_future :any
      c = resolvable_event.resolve

      z2 = zip_events a, b, c
      z3 = zip_events a
      z4 = zip_events

      expect(z2.resolved?).to be_truthy
      expect(z3.resolved?).to be_truthy
      expect(z4.resolved?).to be_truthy
    end
  end

  describe '.rejected_future' do
    it 'raises the correct error when passed an unraised error' do
      f = rejected_future(StandardError.new('boom'))
      expect { f.value! }.to raise_error(StandardError, 'boom')
    end
  end

  describe 'Future' do
    it 'has sync and async callbacks' do
      callbacks_tester = ->(event_or_future) do
        queue     = Queue.new
        push_args = -> *args { queue.push args }

        event_or_future.on_resolution!(&push_args)
        event_or_future.on_resolution!(1, &push_args)
        if event_or_future.is_a? Concurrent::Promises::Future
          event_or_future.on_fulfillment!(&push_args)
          event_or_future.on_fulfillment!(2, &push_args)
          event_or_future.on_rejection!(&push_args)
          event_or_future.on_rejection!(3, &push_args)
        end

        event_or_future.on_resolution(&push_args)
        event_or_future.on_resolution(4, &push_args)
        if event_or_future.is_a? Concurrent::Promises::Future
          event_or_future.on_fulfillment(&push_args)
          event_or_future.on_fulfillment(5, &push_args)
          event_or_future.on_rejection(&push_args)
          event_or_future.on_rejection(6, &push_args)
        end
        event_or_future.on_resolution_using(:io, &push_args)
        event_or_future.on_resolution_using(:io, 7, &push_args)
        if event_or_future.is_a? Concurrent::Promises::Future
          event_or_future.on_fulfillment_using(:io, &push_args)
          event_or_future.on_fulfillment_using(:io, 8, &push_args)
          event_or_future.on_rejection_using(:io, &push_args)
          event_or_future.on_rejection_using(:io, 9, &push_args)
        end

        event_or_future.wait
        ::Array.new(event_or_future.is_a?(Concurrent::Promises::Future) ? 12 : 6) { queue.pop }
      end

      callback_results = callbacks_tester.call(fulfilled_future(:v))
      expect(callback_results).to contain_exactly([true, :v, nil],
                                                  [true, :v, nil, 1],
                                                  [:v],
                                                  [:v, 2],
                                                  [true, :v, nil],
                                                  [true, :v, nil, 4],
                                                  [:v],
                                                  [:v, 5],
                                                  [true, :v, nil],
                                                  [true, :v, nil, 7],
                                                  [:v],
                                                  [:v, 8])

      err              = StandardError.new 'boo'
      callback_results = callbacks_tester.call(rejected_future(err))
      expect(callback_results).to contain_exactly([false, nil, err],
                                                  [false, nil, err, 1],
                                                  [err],
                                                  [err, 3],
                                                  [false, nil, err],
                                                  [false, nil, err, 4],
                                                  [err],
                                                  [err, 6],
                                                  [false, nil, err],
                                                  [false, nil, err, 7],
                                                  [err],
                                                  [err, 9])

      callback_results = callbacks_tester.call(resolved_event)
      expect(callback_results).to contain_exactly([], [1], [], [4], [], [7])
    end

    methods_with_timeout = { wait:   false,
                             wait!:  false,
                             value:  nil,
                             value!: nil,
                             reason: nil,
                             result: nil }
    methods_with_timeout.each do |method_with_timeout, timeout_value|
      it "#{ method_with_timeout } supports setting timeout" do
        start_latch = Concurrent::CountDownLatch.new
        end_latch   = Concurrent::CountDownLatch.new

        future = future do
          start_latch.count_down
          end_latch.wait(0.2)
        end

        expect(start_latch.wait(0.1)).to eq true
        expect(future).not_to be_resolved
        expect(future.send(method_with_timeout, 0.01)).to eq timeout_value
        expect(future).not_to be_resolved

        end_latch.count_down
        expect(future.value!).to eq true
      end
    end

    it 'chains' do
      future0 = future { 1 }.then { |v| v + 2 } # both executed on default FAST_EXECUTOR
      future1 = future0.then_on(:fast) { raise 'boo' } # executed on IO_EXECUTOR
      future2 = future1.then { |v| v + 1 } # will reject with 'boo' error, executed on default FAST_EXECUTOR
      future3 = future1.rescue { |err| err.message } # executed on default FAST_EXECUTOR
      future4 = future0.chain { |success, value, reason| success } # executed on default FAST_EXECUTOR
      future5 = future3.with_default_executor(:fast) # connects new future with different executor, the new future is resolved when future3 is
      future6 = future5.then(&:capitalize) # executes on IO_EXECUTOR because default was set to :io on future5
      future7 = future0 & future3
      future8 = future0.rescue { raise 'never happens' } # future0 fulfills so future8'll have same value as future 0

      futures = [future0, future1, future2, future3, future4, future5, future6, future7, future8]
      futures.each(&:wait)

      table = futures.each_with_index.map do |f, i|
        '%5i %7s %10s %6s %4s %6s' % [i, f.fulfilled?, f.value, f.reason,
                                      (f.promise.executor if f.promise.respond_to?(:executor)),
                                      f.default_executor]
      end.unshift('index success      value reason pool d.pool')

      expect(table.join("\n")).to eq <<-TABLE.gsub(/^\s+\|/, '').strip
        |index success      value reason pool d.pool
        |    0    true          3          io     io
        |    1   false               boo fast     io
        |    2   false               boo   io     io
        |    3    true        boo          io     io
        |    4    true       true          io     io
        |    5    true        boo               fast
        |    6    true        Boo        fast   fast
        |    7    true [3, "boo"]                 io
        |    8    true          3          io     io
      TABLE
    end

    it 'chains with correct arguments' do
      heads   = [future { 1 },
                 future { [2, 3] },
                 fulfilled_future(4),
                 fulfilled_future([5, 6])]
      results = [1,
                 [2, 3],
                 4,
                 [5, 6]]
      heads.each_with_index do |head, i|
        expect(head.then { |a| a }.value!).to eq results[i]
        expect(head.then { |a, b| [a, b].compact }.value!).to eq (results[i].is_a?(Array) ? results[i] : [results[i]])
        expect(head.then { |*a| a }.value!).to eq [results[i]]
      end
    end

    it 'constructs promise like tree' do
      # if head of the tree is not constructed with #future but with #delay it does not start execute,
      # it's triggered later by calling wait or value on any of the dependent futures or the delay itself
      three = (head = delay { 1 }).then { |v| v.succ }.then(&:succ)
      four  = three.delay.then(&:succ)

      # meaningful to_s and inspect defined for Future and Promise
      expect(head.to_s).to match(/#<Concurrent::Promises::Future:0x[\da-f]+ pending>/)
      expect(head.inspect).to(
          match(/#<Concurrent::Promises::Future:0x[\da-f]+ pending>/))

      # evaluates only up to three, four is left unevaluated
      expect(three.value!).to eq 3
      expect(four).not_to be_resolved

      expect(four.value!).to eq 4

      # futures hidden behind two delays trigger evaluation of both
      double_delay = delay { 1 }.delay.then(&:succ)
      expect(double_delay.value!).to eq 2
    end

    it 'allows graphs' do
      head    = future { 1 }
      branch1 = head.then(&:succ)
      branch2 = head.then(&:succ).delay.then(&:succ)
      results = [
          zip(branch1, branch2).then { |b1, b2| b1 + b2 },
          branch1.zip(branch2).then { |b1, b2| b1 + b2 },
          (branch1 & branch2).then { |b1, b2| b1 + b2 }]

      Thread.pass until branch1.resolved?
      expect(branch1).to be_resolved
      expect(branch2).not_to be_resolved

      expect(results.map(&:value)).to eq [5, 5, 5]
      expect(zip(branch1, branch2).value!).to eq [2, 3]
    end

    describe '#flat' do
      it 'returns value of inner future' do
        f = future { future { 1 } }.flat.then(&:succ)
        expect(f.value!).to eq 2
      end

      it 'propagates rejection of inner future' do
        err = StandardError.new('boo')
        f   = future { rejected_future(err) }.flat
        expect(f.reason).to eq err
      end

      it 'it propagates rejection of the future which was suppose to provide inner future' do
        f = future { raise 'boo' }.flat
        expect(f.reason.message).to eq 'boo'
      end

      it 'rejects if inner value is not a future' do
        f = future { 'boo' }.flat
        expect(f.reason).to be_an_instance_of TypeError
      end

      it 'accepts inner event' do
        f = future { resolved_event }.flat
        expect(f.result).to eq [true, nil, nil]
      end

      it 'propagates requests for values to delayed futures' do
        expect(future { delay { 1 } }.flat.value!(0.1)).to eq 1
        expect(::Array.new(3) { |i| Concurrent::Promises.delay { i } }.
            inject { |a, b| a.then { b }.flat }.value!(0.2)).to eq 2
      end

      it 'has shortcuts' do
        expect(fulfilled_future(1).then_flat { |v| future(v) { v + 1 } }.value!).to eq 2
        expect(fulfilled_future(1).then_flat_event { |v| resolved_event }.wait.resolved?).to eq true
        expect(fulfilled_future(1).then_flat_on(:fast) { |v| future(v) { v + 1 } }.value!).to eq 2
      end
    end

    it 'resolves future when Exception raised' do
      message = 'reject by an Exception'
      future  = future { raise Exception, message }
      expect(future.wait(0.1)).to eq true
      future.wait
      expect(future).to be_resolved
      expect(future).to be_rejected

      expect(future.reason).to be_instance_of Exception
      expect(future.result).to be_instance_of Array
      expect(future.value).to be_nil
      expect { future.value! }.to raise_error(Exception, message)
    end

    it 'runs' do
      body = lambda do |v|
        v += 1
        v < 5 ? future(v, &body) : v
      end
      expect(future(0, &body).run.value!).to eq 5

      body = lambda do |v|
        v += 1
        v < 5 ? future(v, &body) : raise(v.to_s)
      end
      expect(future(0, &body).run.reason.message).to eq '5'

      body = lambda do |v|
        v += 1
        v < 5 ? [future(v, &body)] : v
      end
      expect(future(0, &body).run(-> v { v.first if v.is_a? Array }).value!).to eq 5
    end

    it 'can be risen when rejected' do
      strip_methods = -> backtrace do
        backtrace.map do |line|
          /^.*:\d+:in/.match(line)[0] rescue line
        end
      end

      future    = rejected_future TypeError.new
      backtrace = caller; exception = (raise future rescue $!)
      expect(exception).to be_a TypeError
      expect(strip_methods[backtrace] - strip_methods[exception.backtrace]).to be_empty

      exception = TypeError.new
      exception.set_backtrace(first_backtrace = %W[/a /b /c])
      future    = rejected_future exception
      backtrace = caller; exception = (raise future rescue $!)
      expect(exception).to be_a TypeError
      expect(strip_methods[first_backtrace + backtrace] - strip_methods[exception.backtrace]).to be_empty

      future    = rejected_future(TypeError.new) & rejected_future(TypeError.new)
      backtrace = caller; exception = (raise future rescue $!)
      expect(exception).to be_a Concurrent::MultipleErrors
      expect(strip_methods[backtrace] - strip_methods[exception.backtrace]).to be_empty
    end
  end

  describe 'ResolvableEvent' do
    specify "#wait" do
      event = resolvable_event
      expect(event.wait(0, false)).to be_falsey
      expect(event.wait(0, true)).to be_falsey
      expect(event.wait).to eq event
      expect(event.wait(0, false)).to be_truthy
      expect(event.wait(0, true)).to be_truthy
    end

    specify "#resolve(raise_on_reassign = true)" do
      event = resolvable_event
      event.resolve
      expect { event.resolve }.to raise_error(Concurrent::MultipleAssignmentError)
    end

    specify "#resolve(raise_on_reassign = false)" do
      event = resolvable_event
      event.resolve
      expect(event.resolve(false)).to be_falsey
    end

    specify "reservation" do
      event = resolvable_event
      expect(event.reserve).to be_truthy
      expect(event.pending?).to be_truthy
      expect(event.state).to eq :pending
      expect(event.resolve false).to be_falsey
      expect(event.resolve true, true).to be_truthy
    end
  end

  describe 'ResolvableFuture' do
    specify "#wait" do
      future = resolvable_future
      expect(future.wait(0)).to be_falsey
      expect(future.wait(0, [true, :v, nil])).to be_falsey
      expect(future.wait).to eq future
      expect(future.wait(0, nil)).to be_truthy
      expect(future.wait(0, [true, :v, nil])).to be_truthy
    end

    specify "#wait!" do
      future = resolvable_future
      expect(future.wait!(0)).to be_falsey
      expect(future.wait!(0, [true, :v, nil])).to be_falsey
      expect(future.wait!).to eq future
      expect(future.wait!(0, nil)).to be_truthy
      expect(future.wait!(0, [true, :v, nil])).to be_truthy

      future = resolvable_future
      expect(future.wait!(0)).to be_falsey
      expect(future.wait!(0, [false, nil, RuntimeError.new])).to be_falsey
      expect { future.wait! }.to raise_error RuntimeError
    end

    specify "#value" do
      future = resolvable_future
      expect(future.value(0)).to eq nil
      expect(future.value(0, :timeout, [true, :v, nil])).to eq :timeout
      expect(future.value).to eq :v
      expect(future.value(0)).to eq :v
      expect(future.value(0, :timeout, [true, :v, nil])).to eq :v
    end

    specify "#value!" do
      future = resolvable_future
      expect(future.value!(0)).to eq nil
      expect(future.value!(0, :timeout, [true, :v, nil])).to eq :timeout
      expect(future.value!).to eq :v
      expect(future.value!(0, :timeout, nil)).to eq :v
      expect(future.value!(0, :timeout, [true, :v, nil])).to eq :v

      future = resolvable_future
      expect(future.wait!(0)).to be_falsey
      expect(future.wait!(0, [false, nil, RuntimeError.new])).to be_falsey
      expect { future.wait! }.to raise_error RuntimeError
    end

    specify "#reason" do
      future = resolvable_future
      expect(future.reason(0)).to eq nil
      expect(future.reason(0, :timeout, [false, nil, :err])).to eq :timeout
      expect(future.reason).to eq :err
      expect(future.reason(0)).to eq :err
      expect(future.reason(0, :timeout, [false, nil, :err])).to eq :err
    end

    specify "result" do
      future = resolvable_future
      expect(future.result(0)).to eq nil
      expect(future.result(0, [true, :v, nil])).to be_falsey
      expect(future.result).to eq [true, :v, nil]
      expect(future.result(0)).to eq [true, :v, nil]
      expect(future.result(0, [true, :v, nil])).to eq [true, :v, nil]
    end

    specify "reservation" do
      future = resolvable_future
      expect(future.reserve).to be_truthy
      expect(future.pending?).to be_truthy
      expect(future.state).to eq :pending
      expect(future.resolve true, :value, nil, false).to be_falsey
      expect(future.fulfill :value, false).to be_falsey
      expect(future.reject :err, false).to be_falsey
      expect { future.resolve true, :value, nil }.to raise_error(Concurrent::MultipleAssignmentError)
      expect(future.resolve true, :value, nil, false, true).to be_truthy

      future = resolvable_future
      expect(future.reserve).to be_truthy
      expect(future.fulfill :value, false, true).to be_truthy

      future = resolvable_future
      expect(future.reserve).to be_truthy
      expect(future.reject :err, false, true).to be_truthy
    end

    specify "atomic_resolution" do
      future1 = resolvable_future
      future2 = resolvable_future

      expect(Concurrent::Promises::Resolvable.
          atomic_resolution(future1 => [true, :v, nil],
                            future2 => [false, nil, :err])).to eq true
      expect(future1.fulfilled?).to be_truthy
      expect(future2.rejected?).to be_truthy

      future1 = resolvable_future
      future2 = resolvable_future.fulfill :val

      expect(Concurrent::Promises::Resolvable.
          atomic_resolution(future1 => [true, :v, nil],
                            future2 => [false, nil, :err])).to eq false

      expect(future1.pending?).to be_truthy
      expect(future2.fulfilled?).to be_truthy

      expect(future1.reserve).to be_truthy
      expect(future2.reserve).to be_falsey
    end
  end

  describe 'interoperability' do
    it 'with processing actor', if: !Concurrent.on_jruby? do
      require 'concurrent/actor'
      actor = Concurrent::Actor::Utils::AdHoc.spawn :doubler do
        -> v { v * 2 }
      end

      expect(future { 2 }.
          then_ask(actor).
          then { |v| v + 2 }.
          value!).to eq 6
    end

    it 'with erlang actor' do
      require 'concurrent/edge/erlang_actor'
      actor = Concurrent::ErlangActor.spawn type: :on_thread do
        reply receive * 2
      end

      expect(future { 2 }.
          then_ask(actor).
          then { |v| v + 2 }.
          value!).to eq 6
    end

    it 'with channel' do
      require 'concurrent/edge/channel'
      ch1 = Concurrent::Promises::Channel.new
      ch2 = Concurrent::Promises::Channel.new

      result = Concurrent::Promises::Channel.select_op([ch1, ch2])
      ch1.push 1
      expect(result.value!).to eq [ch1, 1]

      future { 1 + 1 }.then_channel_push(ch1)
      result = (Concurrent::Promises.future { '%02d' } & ch1.select_op(ch2)).
          then { |format, (_channel, value)| format format, value }
      expect(result.value!).to eq '02'
    end
  end

  specify 'zip_futures_over' do
    expect(zip_futures_over([1, 2]) { |v| v.succ }.value!).to eq [2, 3]
  end
end
