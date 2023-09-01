require 'concurrent/scheduled_task'
require 'concurrent/atomic/count_down_latch'
require 'timecop'
require_relative 'concern/dereferenceable_shared'
require_relative 'concern/obligation_shared'
require_relative 'concern/observable_shared'

module Concurrent

  RSpec.describe ScheduledTask do

    context 'behavior' do

      let!(:fulfilled_value) { 10 }
      let!(:rejected_reason) { StandardError.new('mojo jojo') }

      let(:pending_subject) do
        ScheduledTask.new(1){ fulfilled_value }.execute
      end

      let(:fulfilled_subject) do
        ScheduledTask.new(0, executor: :immediate){ fulfilled_value }.execute
      end

      let(:rejected_subject) do
        ScheduledTask.new(0, executor: :immediate){ raise rejected_reason }.execute
      end

      def dereferenceable_subject(value, opts = {})
        task = ScheduledTask.execute(0, opts){ value }.execute
        task.value
        task
      end

      def dereferenceable_observable(opts = {})
        ScheduledTask.new(0, opts){ 'value' }
      end

      def execute_dereferenceable(subject)
        subject.execute
        subject.value
      end

      def trigger_observable(observable)
        observable.execute
        sleep(0.2)
      end

      subject{ ScheduledTask.new(0.1){ nil } }

      it_should_behave_like :obligation
      it_should_behave_like :dereferenceable
      it_should_behave_like :observable
    end

    context '#initialize' do

      it 'accepts a number of seconds (from now) as the schedule time' do
        expected = 60
        Timecop.freeze do
          task = ScheduledTask.new(expected){ nil }.execute
          expect(task.initial_delay).to be_within(0.1).of(expected)
        end
      end

      it 'raises an exception when seconds is less than zero' do
        expect {
          ScheduledTask.new(-1){ nil }
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception when no block given' do
        expect {
          ScheduledTask.new(1)
        }.to raise_error(ArgumentError)
      end

      it 'sets the initial state to :unscheduled' do
        task = ScheduledTask.new(1){ nil }
        expect(task).to be_unscheduled
      end
    end

    context 'instance #execute' do

      it 'does nothing unless the state is :unscheduled' do
        expect(Concurrent).not_to receive(:timer).with(any_args)
        task = ScheduledTask.new(1){ nil }
        task.instance_variable_set(:@state, :pending)
        task.execute
        task.instance_variable_set(:@state, :rejected)
        task.execute
        task.instance_variable_set(:@state, :fulfilled)
        task.execute
      end

      it 'sets the sate to :pending' do
        task = ScheduledTask.new(1){ nil }
        task.execute
        expect(task).to be_pending
      end

      it 'returns self' do
        task = ScheduledTask.new(1){ nil }
        expect(task.execute).to eq task
      end
    end

    context 'class #execute' do

      it 'creates a new ScheduledTask' do
        task = ScheduledTask.execute(1){ nil }
        expect(task).to be_a(ScheduledTask)
      end

      it 'passes the block to the new ScheduledTask' do
        @expected = false
        task = ScheduledTask.execute(0.1){ @expected = true }
        task.value(1)
        expect(@expected).to be_truthy
      end

      it 'calls #execute on the new ScheduledTask' do
        task = ScheduledTask.new(0.1){ nil }
        allow(ScheduledTask).to receive(:new).with(any_args).and_return(task)
        expect(task).to receive(:execute).with(no_args)
        ScheduledTask.execute(0.1){ nil }
      end
    end

    context 'execution' do

      it 'passes :args from the options to the block' do
        expected = [1, 2, 3]
        actual = nil
        latch = Concurrent::CountDownLatch.new
        ScheduledTask.execute(0, args: expected) do |*args|
          actual = args
          latch.count_down
        end
        latch.wait(2)
        expect(actual).to eq expected
      end

      it 'uses the :executor from the options' do
        latch = Concurrent::CountDownLatch.new
        executor = Concurrent::ImmediateExecutor.new
        expect(executor).to receive(:post).once.with(any_args).and_call_original
        ScheduledTask.execute(0, executor: executor) do
          latch.count_down
        end
        latch.wait(2)
      end

      it 'uses the :timer_set from the options' do
        timer = Concurrent::TimerSet.new
        queue = timer.instance_variable_get(:@queue)
        task = ScheduledTask.execute(1, timer_set: timer){ nil }
        expect(queue.size).to eq 1
        task.cancel
      end

      it 'sets the state to :processing when the task is running' do
        start_latch = Concurrent::CountDownLatch.new(1)
        continue_latch = Concurrent::CountDownLatch.new(1)
        task = ScheduledTask.new(0.1) {
          start_latch.count_down
          continue_latch.wait(2)
        }.execute
        start_latch.wait(2)
        state = task.state
        continue_latch.count_down
        expect(state).to eq :processing
      end
    end

    context '#cancel' do

      it 'returns false if the task has already been performed' do
        task = ScheduledTask.new(0.1){ 42 }.execute
        task.value(1)
        expect(task.cancel).to be_falsey
      end

      it 'returns false if the task is already in progress' do
        latch = Concurrent::CountDownLatch.new(1)
        task = ScheduledTask.new(0.1) {
          latch.count_down
          sleep(1)
        }.execute
        latch.wait(1)
        expect(task.cancel).to be_falsey
      end

      it 'cancels the task if it has not yet scheduled' do
        latch = Concurrent::CountDownLatch.new(1)
        task = ScheduledTask.new(0.1){ latch.count_down }
        task.cancel
        task.execute
        expect(latch.wait(0.3)).to be_falsey
      end

      it 'cancels the task if it has not yet started' do
        latch = Concurrent::CountDownLatch.new(1)
        task = ScheduledTask.new(0.3){ latch.count_down }.execute
        sleep(0.1)
        task.cancel
        expect(latch.wait(0.5)).to be_falsey
      end

      it 'returns true on success' do
        task = ScheduledTask.new(10){ nil }.execute
        sleep(0.1)
        expect(task.cancel).to be_truthy
      end

      it 'sets the reason to CancelledOperationError when cancelled' do
        task = ScheduledTask.new(10){ 42 }.execute
        sleep(0.1)
        task.cancel
        expect(task).to be_rejected
        expect(task.reason).to be_a CancelledOperationError
      end
    end

    context 'observation' do

      let(:clazz) do
        Class.new do
          attr_reader :value
          attr_reader :reason
          attr_reader :count
          attr_reader :latch
          def initialize
            @latch = Concurrent::CountDownLatch.new(1)
            @count = 0
          end
          def update(time, value, reason)
            @count = @count.to_i + 1
            @value = value
            @reason = reason
            @latch.count_down
          end
        end
      end

      let(:observer) { clazz.new }

      it 'returns true for an observer added while :unscheduled' do
        task = ScheduledTask.new(0.1){ 42 }
        expect(task.add_observer(observer)).to be_truthy
      end

      it 'returns true for an observer added while :pending' do
        task = ScheduledTask.new(0.1){ 42 }.execute
        expect(task.add_observer(observer)).to be_truthy
      end

      it 'returns true for an observer added while :processing' do
        task = ScheduledTask.new(0.1){ sleep(1); 42 }.execute
        sleep(0.2)
        expect(task.add_observer(observer)).to be_truthy
      end

      it 'notifies all observers on fulfillment' do
        task = ScheduledTask.new(0.1){ 42 }.execute
        task.add_observer(observer)
        observer.latch.wait(1)
        expect(observer.value).to eq(42)
        expect(observer.reason).to be_nil
      end

      it 'notifies all observers on rejection' do
        task = ScheduledTask.new(0.1){ raise StandardError }.execute
        task.add_observer(observer)
        observer.latch.wait(1)
        expect(observer.value).to be_nil
        expect(observer.reason).to be_a(StandardError)
      end
    end
  end
end
