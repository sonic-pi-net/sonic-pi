require 'concurrent/future'
require 'concurrent/executor/simple_executor_service'
require 'concurrent/executor/single_thread_executor'
require 'concurrent/atomic/count_down_latch'

require_relative 'ivar_shared'
require_relative 'thread_arguments_shared'

module Concurrent

  RSpec.describe Future do

    let!(:value) { 10 }
    let(:executor) { SimpleExecutorService.new }

    subject do
      Future.new(executor: executor){
        value
      }.execute.tap{ sleep(0.1) }
    end

    let!(:fulfilled_value) { 10 }
    let!(:rejected_reason) { StandardError.new('mojo jojo') }

    let(:pending_subject) do
      executor = Concurrent::SingleThreadExecutor.new
      executor.post{ sleep(5) }
      Future.execute(executor: executor){ fulfilled_value }
    end

    let(:fulfilled_subject) do
      Future.new(executor: :immediate){ fulfilled_value }.execute
    end

    let(:rejected_subject) do
      Future.new(executor: :immediate){ raise rejected_reason }.execute
    end

    it_should_behave_like :ivar do
      subject { Future.new(executor: :immediate){ value } }

      def dereferenceable_subject(value, opts = {})
        opts = opts.merge(executor: executor)
        Future.new(opts){ value }.execute.tap{ sleep(0.1) }
      end

      def dereferenceable_observable(opts = {})
        opts = opts.merge(executor: executor)
        Future.new(opts){ 'value' }
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
        Concurrent::Future.execute{|*args| args }
      end

      def get_ivar_from_args(opts)
        Concurrent::Future.execute(opts){|*args| args }
      end
    end

    context '#initialize' do

      let(:executor) { ImmediateExecutor.new }

      it 'sets the state to :unscheduled' do
        expect(Future.new(executor: executor){ nil }).to be_unscheduled
      end

      it 'raises an exception when no block given' do
        expect {
          Future.new.execute
        }.to raise_error(ArgumentError)
      end

      it 'uses the executor given with the :executor option' do
        expect(executor).to receive(:post)
        Future.execute(executor: executor){ nil }
      end

      it 'uses the global io executor by default' do
        allow(Concurrent).to receive(:global_io_executor).and_return(executor)
        expect(executor).to receive(:post).and_call_original
        Future.execute{ nil }
      end
    end

    context 'instance #execute' do

      it 'does nothing unless the state is :unscheduled' do
        executor = ImmediateExecutor.new
        expect(executor).not_to receive(:post).with(any_args)
        future = Future.new(executor: executor){ nil }
        future.instance_variable_set(:@state, :pending)
        future.execute
        future.instance_variable_set(:@state, :rejected)
        future.execute
        future.instance_variable_set(:@state, :fulfilled)
        future.execute
      end

      it 'posts the block given on construction' do
        expect(executor).to receive(:post).with(any_args)
        future = Future.new(executor: executor){ nil }
        future.execute
      end

      it 'sets the state to :pending' do
        latch = Concurrent::CountDownLatch.new
        executor = Concurrent::SingleThreadExecutor.new
        executor.post{ latch.wait(2) }

        future = Future.new(executor: executor){ 42 }
        future.execute
        expect(future).to be_pending
        latch.count_down
      end

      it 'returns self' do
        future = Future.new(executor: executor){ nil }
        expect(future.execute).to be future
      end
    end

    context 'class #execute' do

      let(:executor) { ImmediateExecutor.new }

      it 'creates a new Future' do
        future = Future.execute(executor: executor){ nil }
        expect(future).to be_a(Future)
      end

      it 'passes the block to the new Future' do
        @expected = false
        Future.execute(executor: executor){ @expected = true }
        expect(@expected).to be_truthy
      end

      it 'calls #execute on the new Future' do
        future = double('future')
        allow(Future).to receive(:new).with(any_args).and_return(future)
        expect(future).to receive(:execute).with(no_args)
        Future.execute{ nil }
      end
    end

    context 'fulfillment' do

      let(:executor) { ImmediateExecutor.new }

      it 'sets the state to :processing while the task is executing' do
        start_latch = Concurrent::CountDownLatch.new
        continue_latch = Concurrent::CountDownLatch.new
        executor = Concurrent::SingleThreadExecutor.new

        future = Future.execute(executor: executor) do
          start_latch.count_down
          continue_latch.wait(2)
          42
        end

        start_latch.wait(2)
        state = future.state
        continue_latch.count_down
        future.value

        expect(state).to eq :processing
      end

      it 'passes all arguments to handler' do
        expected = false
        Future.new(executor: executor){ expected = true }.execute
        expect(expected).to be_truthy
      end

      it 'sets the value to the result of the handler' do
        future = Future.new(executor: executor){ 42 }.execute
        expect(future.value).to eq 42
      end

      it 'sets the state to :fulfilled when the block completes' do
        future = Future.new(executor: executor){ 42 }.execute
        expect(future).to be_fulfilled
      end

      it 'sets the value to nil when the handler raises an exception' do
        future = Future.new(executor: executor){ raise StandardError }.execute
        expect(future.value).to be_nil
      end

      it 'sets the value to nil when the handler raises Exception' do
        future = Future.new(executor: executor){ raise Exception }.execute
        expect(future.value).to be_nil
      end

      it 'sets the reason to the Exception instance when the handler raises Exception' do
        future = Future.new(executor: executor){ raise Exception }.execute
        expect(future.reason).to be_a(Exception)
      end

      it 'sets the state to :rejected when the handler raises an exception' do
        future = Future.new(executor: executor){ raise StandardError }.execute
        expect(future).to be_rejected
      end

      context 'aliases' do

        it 'aliases #realized? for #fulfilled?' do
          expect(subject).to be_realized
        end

        it 'aliases #deref for #value' do
          expect(subject.deref).to eq value
        end
      end
    end

    context 'cancellation' do

      context '#cancel' do

        it 'fails to cancel the task once processing has begun' do
          start_latch = Concurrent::CountDownLatch.new
          continue_latch = Concurrent::CountDownLatch.new
          f = Future.execute do
            start_latch.count_down
            continue_latch.wait(2)
            42
          end

          start_latch.wait(2)
          cancelled = f.cancel
          continue_latch.count_down

          expect(cancelled).to be false
          expect(f.value).to eq 42
          expect(f).to be_fulfilled
        end

        it 'fails to cancel the task once processing is complete' do
          f = Future.execute{ 42 }
          f.wait
          cancelled = f.cancel

          expect(cancelled).to be false
          expect(f.value).to eq 42
          expect(f).to be_fulfilled
        end

        it 'cancels a pending task' do
          executor = Concurrent::SingleThreadExecutor.new
          latch = Concurrent::CountDownLatch.new
          executor.post{ latch.wait(2) }

          f = Future.execute(executor: executor){ 42 }
          cancelled = f.cancel
          latch.count_down

          expect(cancelled).to be true
          expect(f.value).to be_nil
          expect(f).to be_rejected
          expect(f.reason).to be_a Concurrent::CancelledOperationError
        end
      end

      context '#wait_or_cancel' do

        it 'returns true if the operation completes before timeout' do
          f = Future.execute{ 42 }
          success = f.wait_or_cancel(1)

          expect(success).to be true
          expect(f.value).to eq 42
          expect(f).to be_fulfilled
        end

        it 'cancels the task on timeout' do
          latch = Concurrent::CountDownLatch.new
          executor = Concurrent::SingleThreadExecutor.new
          executor.post{ latch.wait(2) }

          f = Future.execute(executor: executor){ 42 }
          success = f.wait_or_cancel(0.1)
          latch.count_down

          expect(success).to be false
          expect(f.value).to be_nil
          expect(f).to be_rejected
          expect(f.reason).to be_a Concurrent::CancelledOperationError
        end
      end
    end

    context 'observation' do

      let(:executor) { ImmediateExecutor.new }

      let(:clazz) do
        Class.new do
          attr_reader :value
          attr_reader :reason
          attr_reader :count
          define_method(:update) do |time, value, reason|
            @count ||= 0
            @count = @count.to_i + 1
            @value = value
            @reason = reason
          end
        end
      end

      let(:observer) { clazz.new }

      it 'notifies all observers on fulfillment' do
        future = Future.new(executor: executor){ 42 }
        future.add_observer(observer)

        future.execute

        expect(observer.value).to eq(42)
        expect(observer.reason).to be_nil
      end

      it 'notifies all observers on rejection' do
        future = Future.new(executor: executor){ raise StandardError }
        future.add_observer(observer)

        future.execute

        expect(observer.value).to be_nil
        expect(observer.reason).to be_a(StandardError)
      end

      it 'notifies an observer added after fulfillment' do
        future = Future.new(executor: executor){ 42 }.execute
        future.add_observer(observer)
        expect(observer.value).to eq(42)
      end

      it 'notifies an observer added after rejection' do
        future = Future.new(executor: executor){ raise StandardError }.execute
        future.add_observer(observer)
        expect(observer.reason).to be_a(StandardError)
      end

      it 'does not notify existing observers when a new observer added after fulfillment' do
        future = Future.new(executor: executor){ 42 }.execute
        future.add_observer(observer)

        expect(observer.count).to eq(1)

        o2 = clazz.new
        future.add_observer(o2)

        expect(observer.count).to eq(1)
        expect(o2.value).to eq(42)
      end

      it 'does not notify existing observers when a new observer added after rejection' do
        future = Future.new(executor: executor){ raise StandardError }.execute
        future.add_observer(observer)

        expect(observer.count).to eq(1)

        o2 = clazz.new
        future.add_observer(o2)

        expect(observer.count).to eq(1)
        expect(o2.reason).to be_a(StandardError)
      end

      context 'deadlock avoidance' do

        def reentrant_observer(future)
          obs = ::Object.new
          obs.define_singleton_method(:update) do |time, value, reason|
            @value = future.value
          end
          obs.define_singleton_method(:value) { @value }
          obs
        end

        it 'should notify observers outside mutex lock' do
          future = Future.new(executor: executor){ 42 }
          obs = reentrant_observer(future)

          future.add_observer(obs)
          future.execute

          expect(obs.value).to eq 42
        end

        it 'should notify a new observer added after fulfillment outside lock' do
          future = Future.new(executor: executor){ 42 }.execute
          obs = reentrant_observer(future)

          future.add_observer(obs)

          expect(obs.value).to eq 42
        end
      end
    end
  end
end
