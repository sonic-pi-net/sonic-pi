require 'concurrent/promise'
require 'concurrent/executor/simple_executor_service'
require 'concurrent/executor/single_thread_executor'
require 'concurrent/atomic/count_down_latch'

require_relative 'ivar_shared'
require_relative 'thread_arguments_shared'

module Concurrent

  RSpec.describe Promise do

    let!(:value) { 10 }
    let(:executor) { SimpleExecutorService.new }

    let(:empty_root) { Promise.new(executor: executor){ nil } }
    let!(:fulfilled_value) { 10 }
    let!(:rejected_reason) { StandardError.new('mojo jojo') }

    let(:pending_subject) do
      executor = Concurrent::SingleThreadExecutor.new
      executor.post{ sleep(5) }
      Promise.execute(executor: executor){ fulfilled_value }
    end

    let(:fulfilled_subject) do
      Promise.new(executor: :immediate){ fulfilled_value }.execute
    end

    let(:rejected_subject) do
      Promise.new(executor: :immediate){ raise rejected_reason }.execute
    end

    it_should_behave_like :ivar do
      subject{ Promise.new(executor: :immediate){ value } }

      def dereferenceable_subject(value, opts = {})
        opts = opts.merge(executor: executor)
        Promise.new(opts){ value }.execute.tap{ sleep(0.1) }
      end

      def dereferenceable_observable(opts = {})
        opts = opts.merge(executor: executor)
        Promise.new(opts){ 'value' }
      end

      def execute_dereferenceable(subject)
        subject.execute
        sleep(0.1)
      end

      def trigger_observable(observable)
        observable.execute
        sleep(0.1)
      end
    end

    it_should_behave_like :thread_arguments do

      def get_ivar_from_no_args
        Concurrent::Promise.execute{|*args| args }
      end

      def get_ivar_from_args(opts)
        Concurrent::Promise.execute(opts){|*args| args }
      end
    end

    context 'initializers' do
      describe '.fulfill' do

        subject { Promise.fulfill(10) }

        it 'should return a Promise' do
          expect(subject).to be_a Promise
        end

        it 'should return a fulfilled Promise' do
          expect(subject).to be_fulfilled
        end

        it 'should return a Promise with set value' do
          expect(subject.value).to eq 10
        end
      end

      describe '.reject' do

        let(:reason) { ArgumentError.new }
        subject { Promise.reject(reason) }

        it 'should return a Promise' do
          expect(subject).to be_a Promise
        end

        it 'should return a rejected Promise' do
          expect(subject).to be_rejected
        end

        it 'should return a Promise with set reason' do
          expect(subject.reason).to be reason
        end
      end

      describe '.new' do
        it 'should return an unscheduled Promise' do
          p = Promise.new(executor: :immediate){ nil }
          expect(p).to be_unscheduled
        end
      end

      describe '.execute' do
        it 'creates a new Promise' do
          p = Promise.execute(executor: :immediate){ nil }
          expect(p).to be_a(Promise)
        end

        it 'passes the block to the new Promise' do
          p = Promise.execute(executor: :immediate){ 20 }
          expect(p.value).to eq 20
        end

        it 'calls #execute on the new Promise' do
          p = double('promise')
          allow(Promise).to receive(:new).with(any_args).and_return(p)
          expect(p).to receive(:execute).with(no_args)
          Promise.execute(executor: :immediate){ nil }
        end
      end
    end

    context '#execute' do

      context 'unscheduled' do

        it 'sets the promise to :pending' do
          start_latch = CountDownLatch.new
          end_latch = CountDownLatch.new
          p = Promise.new(executor: executor) do
            start_latch.count_down
            end_latch.wait(1)
          end
          start_latch.wait(1)
          p.execute
          expect(p).to be_pending
          end_latch.count_down
        end

        it 'posts the block given in construction' do
          executor = ImmediateExecutor.new
          expect(executor).to receive(:post).with(any_args).and_call_original
          Promise.new(executor: executor){ nil }.execute
        end
      end

      context 'pending' do

        it 'sets the promise to :pending' do
          latch = CountDownLatch.new
          p = Promise.new{ latch.wait(1) }.execute
          expect(p).to be_pending
          latch.count_down
        end

        it 'does not post again' do
          executor = SimpleExecutorService.new
          expect(executor).to receive(:post).with(any_args).once

          latch = CountDownLatch.new
          p = Promise.new(executor: executor){ latch.wait(1) }.execute

          10.times { p.execute }
          latch.count_down
        end
      end

      describe 'with children' do

        let(:start_latch) { CountDownLatch.new(4) }
        let(:end_latch) { CountDownLatch.new(1) }
        let(:root) { Promise.new(executor: executor){ start_latch.count_down; end_latch.wait(5) } }
        let(:c1) { root.then { start_latch.count_down; end_latch.wait(5) } }
        let(:c2) { root.then { start_latch.count_down; end_latch.wait(5) } }
        let(:c2_1) { c2.then { start_latch.count_down; end_latch.wait(5) } }

        context 'when called on the root' do
          it 'should set all promises to :pending' do
            root.execute
            start_latch.wait(1)
            [root, c1, c2, c2_1].each { |p| expect(p).to be_pending }
            end_latch.count_down
          end
        end

        context 'when called on a child' do
          it 'should set all promises to :pending' do
            c2_1.execute
            start_latch.wait(1)
            [root, c1, c2, c2_1].each { |p| expect(p).to be_pending }
            end_latch.count_down
          end
        end

        context "when called on child after parent completes" do
          let(:parent_promise) { Concurrent::Promise.execute { 1 + 1 }.tap { |p| p.wait } }
          it 'sets state to :pending immediately' do
            latch = CountDownLatch.new
            child_promise = parent_promise.then { |_| latch.wait }.execute
            expect(child_promise.state).to eq(:pending)
            latch.count_down
          end
        end
      end
    end

    describe '#then' do

      it 'returns a new promise when a block is passed' do
        child = empty_root.then { nil }
        expect(child).to be_a Promise
        expect(child).not_to be empty_root
      end

      it 'returns a new promise when a rescuer is passed' do
        child = empty_root.then(Proc.new{})
        expect(child).to be_a Promise
        expect(child).not_to be empty_root
      end

      it 'returns a new promise when a block and rescuer are passed' do
        child = empty_root.then(Proc.new{}) { nil }
        expect(child).to be_a Promise
        expect(child).not_to be empty_root
      end

      it 'returns a new promise when a block, rescuer and executor are passed' do
        new_executor = Concurrent::SingleThreadExecutor.new
        child = empty_root.then(Proc.new{}, new_executor) { nil }
        expect(child).to be_a Promise
        expect(child).not_to be empty_root
        expect(child.instance_variable_get(:@executor)).to be(new_executor)
      end

      it 'supports setting the executor using a named parameter' do
        new_executor = Concurrent::SingleThreadExecutor.new
        child = empty_root.then(executor: new_executor) { nil }
        expect(child.instance_variable_get(:@executor)).to be(new_executor)
      end

      it 'should have block or rescuers' do
        expect { empty_root.then }.to raise_error(ArgumentError)
      end

      context 'unscheduled' do

        let(:p1) { Promise.new(executor: executor){nil} }
        let(:child) { p1.then{} }

        it 'returns a new promise' do
          expect(child).to be_a Promise
          expect(p1).not_to be child
        end

        it 'returns an unscheduled promise' do
          expect(child).to be_unscheduled
        end
      end

      context 'pending' do

        let(:child) { pending_subject.then{} }

        it 'returns a new promise' do
          expect(child).to be_a Promise
          expect(pending_subject).not_to be child
        end

        it 'returns a pending promise' do
          expect(child).to be_pending
        end
      end

      context 'fulfilled' do
        it 'returns a new Promise' do
          p1 = fulfilled_subject
          p2 = p1.then{}
          expect(p2).to be_a(Promise)
          expect(p1).not_to eq p2
        end

        it 'notifies fulfillment to new child' do
          child = fulfilled_subject.then(Proc.new{ 7 }) { |v| v + 5 }
          expect(child.value).to eq fulfilled_value + 5
        end
      end

      context 'rejected' do
        it 'returns a new Promise when :rejected' do
          p1 = rejected_subject
          p2 = p1.then{}
          expect(p2).to be_a(Promise)
          expect(p1).not_to eq p2
        end

        it 'notifies rejection to new child' do
          child = rejected_subject.then(Proc.new{ 7 }) { |v| v + 5 }
          expect(child.value).to eq 7
        end
      end

      it 'can be called more than once' do
        p = pending_subject
        p1 = p.then{}
        p2 = p.then{}
        expect(p1).not_to be p2
      end
    end

    describe 'on_success' do
      it 'should have a block' do
        expect { empty_root.on_success }.to raise_error(ArgumentError)
      end

      it 'returns a new promise' do
        child = empty_root.on_success { nil }
        expect(child).to be_a Promise
        expect(child).not_to be empty_root
      end
    end

    context '#rescue' do

      it 'returns a new promise' do
        child = empty_root.rescue { nil }
        expect(child).to be_a Promise
        expect(child).not_to be empty_root
      end
    end

    describe '#flat_map' do

      it 'returns a promise' do
        child = empty_root.flat_map { nil }
        expect(child).to be_a Promise
        expect(child).not_to be empty_root
      end

      it 'succeeds if both promises succeed' do
        child = Promise.new(executor: :immediate) { 1 }.
          flat_map { |v| Promise.new(executor: :immediate) { v + 10 } }.execute.wait

        expect(child.value!).to eq(11)
      end

      it 'fails if the left promise fails' do
        child = Promise.new(executor: :immediate) { fail }.
          flat_map { |v| Promise.new(executor: :immediate) { v + 10 } }.execute.wait

        expect(child).to be_rejected
      end

      it 'fails if the right promise fails' do
        child = Promise.new(executor: :immediate) { 1 }.
          flat_map { |v| Promise.new(executor: :immediate) { fail } }.execute.wait

        expect(child).to be_rejected
      end

      it 'fails if the generating block fails' do
        child = Promise.new(executor: :immediate) { }.flat_map { fail }.execute.wait

        expect(child).to be_rejected
      end

    end

    describe '#zip' do
      let(:promise1) { Promise.new(executor: :immediate) { 1 } }
      let(:promise2) { Promise.new(executor: :immediate) { 2 } }
      let(:promise3) { Promise.new(executor: :immediate) { [3] } }

      it 'executes the returned Promise by default' do
        composite = promise1.zip(promise2, promise3)

        expect(composite).to be_fulfilled
      end

      it 'executes the returned Promise when execute is true' do
        composite = promise1.zip(promise2, promise3, execute: true)

        expect(composite).to be_fulfilled
      end

      it 'does not execute the returned Promise when execute is false' do
        composite = promise1.zip(promise2, promise3, execute: false)

        expect(composite).to be_unscheduled
      end

      it 'allows setting executor for Promise chain' do
        new_executor = Concurrent::SingleThreadExecutor.new
        promise = promise1.zip(promise2, promise3, executor: new_executor)

        promise = promise.instance_variable_get(:@parent) until promise.send(:root?)
        expect(promise.instance_variable_get(:@executor)).to be(new_executor)
      end

      it 'yields the results as an array' do
        composite = promise1.zip(promise2, promise3).execute.wait

        expect(composite.value).to eq([1, 2, [3]])
      end

      it 'fails if one component fails' do
        composite = promise1.zip(promise2, rejected_subject, promise3).execute.wait

        expect(composite).to be_rejected
      end

      it 'preserves ordering of the executed promises' do
        10.times do
          latch1 = CountDownLatch.new
          latch2 = CountDownLatch.new
          executor = SimpleExecutorService.new

          p1 = Concurrent::Promise.execute(executor: executor) { latch1.wait; 'one' }
          p2 = Concurrent::Promise.execute(executor: executor) { latch2.wait; 'two' }
          p3 = Concurrent::Promise.execute(executor: executor) { 'three' }

          latch1.count_down
          latch2.count_down

          result = Concurrent::Promise.zip(p1, p2, p3).value!
          expect(result).to eq(['one', 'two', 'three'])
        end
      end
    end

    describe '.zip' do
      let(:promise1) { Promise.new(executor: :immediate) { 1 } }
      let(:promise2) { Promise.new(executor: :immediate) { 2 } }
      let(:promise3) { Promise.new(executor: :immediate) { [3] } }

      it 'executes the returned Promise by default' do
        composite = Promise.zip(promise1, promise2, promise3)

        expect(composite).to be_fulfilled
      end

      it 'executes the returned Promise when execute is true' do
        composite = Promise.zip(promise1, promise2, promise3, execute: true)

        expect(composite).to be_fulfilled
      end

      it 'does not execute the returned Promise when execute is false' do
        composite = Promise.zip(promise1, promise2, promise3, execute: false)

        expect(composite).to be_unscheduled
      end

      it 'allows setting executor for Promise chain' do
        new_executor = Concurrent::SingleThreadExecutor.new
        promise = Promise.zip(promise1, promise2, promise3, executor: new_executor)

        promise = promise.instance_variable_get(:@parent) until promise.send(:root?)
        expect(promise.instance_variable_get(:@executor)).to be(new_executor)
      end

      it 'yields the results as an array' do
        composite = Promise.zip(promise1, promise2, promise3).execute.wait

        expect(composite.value).to eq([1, 2, [3]])
      end

      it 'fails if one component fails' do
        composite = Promise.zip(promise1, promise2, rejected_subject, promise3).execute.wait

        expect(composite).to be_rejected
      end

      it 'preserves ordering of the executed promises' do
        promise1 = Promise.execute do
          # resolves after the second promise
          sleep 0.2
          'one'
        end

        promise2 = Promise.execute do
          sleep 0.1
          'two'
        end

        promise3 = Promise.execute do
          'three'
        end

        result = Promise.zip(promise1, promise2, promise3).value

        expect(result).to eql(['one', 'two', 'three'])
      end
    end

    describe 'aggregators' do

      let(:promise1) { Promise.new(executor: :immediate) { 1 } }
      let(:promise2) { Promise.new(executor: :immediate) { 2 } }
      let(:promise3) { Promise.new(executor: :immediate) { [3] } }

      describe '.all?' do

        it 'returns a new Promise' do
          composite = Promise.all?(promise1, promise2, promise3).execute
          expect(composite).to be_a Concurrent::Promise
        end

        it 'does not execute the returned Promise' do
          composite = Promise.all?(promise1, promise2, promise3)
          expect(composite).to be_unscheduled
        end

        it 'executes the #then condition when all components succeed' do
          counter = Concurrent::AtomicFixnum.new(0)
          latch = Concurrent::CountDownLatch.new(1)

          Promise.all?(promise1, promise2, promise3).
            then { counter.up; latch.count_down }.
            rescue { counter.down; latch.count_down }.
          execute

          latch.wait(1)

          expect(counter.value).to eq 1
        end

        it 'executes the #then condition when no promises are given' do
          counter = Concurrent::AtomicFixnum.new(0)
          latch = Concurrent::CountDownLatch.new(1)

          Promise.all?.
            then { counter.up; latch.count_down }.
            rescue { counter.down; latch.count_down }.
          execute

          latch.wait(1)

          expect(counter.value).to eq 1
        end

        it 'executes the #rescue handler if even one component fails' do
          counter = Concurrent::AtomicFixnum.new(0)
          latch = Concurrent::CountDownLatch.new(1)

          Promise.all?(promise1, promise2, rejected_subject, promise3).
            then { counter.up; latch.count_down }.
            rescue { counter.down; latch.count_down }.
          execute

          latch.wait(1)

          expect(counter.value).to eq(-1)
        end
      end

      describe '.any?' do

        it 'returns a new Promise' do
          composite = Promise.any?(promise1, promise2, promise3).execute
          expect(composite).to be_a Concurrent::Promise
        end

        it 'does not execute the returned Promise' do
          composite = Promise.any?(promise1, promise2, promise3)
          expect(composite).to be_unscheduled
        end

        it 'executes the #then condition when any components succeed' do
          counter = Concurrent::AtomicFixnum.new(0)
          latch = Concurrent::CountDownLatch.new(1)

          Promise.any?(promise1, promise2, rejected_subject, promise3).
            then { counter.up; latch.count_down }.
            rescue { counter.down; latch.count_down }.
          execute

          latch.wait(1)

          expect(counter.value).to eq 1
        end

        it 'executes the #then condition when no promises are given' do
          counter = Concurrent::AtomicFixnum.new(0)
          latch = Concurrent::CountDownLatch.new(1)

          Promise.any?.
            then { counter.up; latch.count_down }.
            rescue { counter.down; latch.count_down }.
          execute

          latch.wait(1)

          expect(counter.value).to eq 1
        end

        it 'executes the #rescue handler if all componenst fail' do
          counter = Concurrent::AtomicFixnum.new(0)
          latch = Concurrent::CountDownLatch.new(1)

          Promise.any?(rejected_subject, rejected_subject, rejected_subject, rejected_subject).
            then { counter.up; latch.count_down }.
            rescue { counter.down; latch.count_down }.
          execute

          latch.wait(1)

          expect(counter.value).to eq(-1)
        end
      end
    end

    context 'fulfillment' do

      context '#set' do

        it '#can only be called on the root promise' do
          root = Promise.new{ :foo }
          child = root.then{ :bar }

          expect { child.set('foo') }.to raise_error PromiseExecutionError
          expect { root.set('foo') }.not_to raise_error
        end

        it 'triggers children' do
          expected = nil
          root = Promise.new(executor: :immediate){ nil }
          root.then{ |result| expected = result }
          root.set(20)
          expect(expected).to eq 20
        end

        it 'can be called with a block' do
          p = Promise.new(executor: :immediate)
          ch = p.then(&:to_s)
          p.set { :value }

          expect(p.value).to eq :value
          expect(p.state).to eq :fulfilled

          expect(ch.value).to eq 'value'
          expect(ch.state).to eq :fulfilled
        end
      end

      context '#fail' do

        it 'can only be called on the root promise' do
          root = Promise.new{ :foo }
          child = root.then{ :bar }

          expect { child.fail }.to raise_error PromiseExecutionError
          expect { root.fail }.not_to raise_error
        end

        it 'rejects children' do
          expected = nil
          root = Promise.new(executor: :immediate)
          root.then(Proc.new{ |reason| expected = reason })
          root.fail(ArgumentError.new('simulated error'))
          expect(expected).to be_a ArgumentError
        end
      end

      it 'passes the result of each block to all its children' do
        expected = nil
        Promise.new(executor: :immediate){ 20 }.then{ |result| expected = result }.execute
        expect(expected).to eq 20
      end

      it 'sets the promise value to the result if its block' do
        root = Promise.new(executor: :immediate){ 20 }
        p = root.then{ |result| result * 2}.execute
        expect(root.value).to eq 20
        expect(p.value).to eq 40
      end

      it 'sets the promise state to :fulfilled if the block completes' do
        p = Promise.new(executor: :immediate){ 10 * 2 }.then{|result| result * 2}.execute
        expect(p).to be_fulfilled
      end

      it 'passes the last result through when a promise has no block' do
        expected = nil
        Promise.new(executor: :immediate){ 20 }.then(Proc.new{}).then{|result| expected = result}.execute
        expect(expected).to eq 20
      end

      it 'uses result as fulfillment value when a promise has no block' do
        p = Promise.new(executor: :immediate){ 20 }.then(Proc.new{}).execute
        expect(p.value).to eq 20
      end

      it 'can manage long chain' do
        root = Promise.new(executor: :immediate){ 20 }
        p1 = root.then { |b| b * 3 }
        p2 = root.then { |c| c + 2 }
        p3 = p1.then { |d| d + 7 }

        root.execute

        expect(root.value).to eq 20
        expect(p1.value).to eq 60
        expect(p2.value).to eq 22
        expect(p3.value).to eq 67
      end
    end

    context 'rejection' do

      it 'passes the reason to all its children' do
        expected = nil
        handler = proc { |reason| expected = reason }
        Promise.new(executor: :immediate){ raise ArgumentError }.then(handler).execute
        expect(expected).to be_a ArgumentError
      end

      it 'sets the promise value to the result if its block' do
        root = Promise.new(executor: :immediate){ raise ArgumentError }
        p = root.then(Proc.new{ |reason| 42 }).execute
        expect(p.value).to eq 42
      end

      it 'sets the promise state to :rejected if the block completes' do
        p = Promise.new(executor: :immediate){ raise ArgumentError }.execute
        expect(p).to be_rejected
      end

      it 'uses reason as rejection reason when a promise has no rescue callable' do
        p = Promise.new(executor: :immediate){ raise ArgumentError }.then{ |val| val }.execute
        expect(p).to be_rejected
        expect(p.reason).to be_a ArgumentError
      end

      it 'rejects on Exception' do
        p = Promise.new(executor: :immediate){ raise Exception }.execute
        expect(p).to be_rejected
      end

    end

    context 'aliases' do

      it 'aliases #realized? for #fulfilled?' do
        expect(fulfilled_subject).to be_realized
      end

      it 'aliases #deref for #value' do
        expect(fulfilled_subject.deref).to eq fulfilled_value
      end

      it 'aliases #catch for #rescue' do
        child = rejected_subject.catch { 7 }
        expect(child.value).to eq 7
      end

      it 'aliases #on_error for #rescue' do
        child = rejected_subject.on_error { 7 }
        expect(child.value).to eq 7
      end
    end
  end
end
