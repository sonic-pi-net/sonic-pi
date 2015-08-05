require_relative 'concern/dereferenceable_shared'
require_relative 'concern/observable_shared'

module Concurrent

  describe Agent do

    let(:executor) { SimpleExecutorService.new }

    subject { Agent.new(0, executor: executor) }

    after(:each) do
      executor.kill
    end

    let(:observer) do
      Class.new do
        attr_reader :value
        define_method(:update) do |time, value|
          @value = value
        end
      end.new
    end

    context '#post_off' do
      subject { Agent.new 2, executor: executor }

      it 'executes post and post-off in order' do
        subject.post { |v| v + 2 }
        subject.post_off { |v| v * 3 }
        subject.await
        expect(subject.value).to eq 12
      end

      it 'times out' do
        ex      = nil
        timeout = false
        subject.rescue(Concurrent::TimeoutError) { timeout = true }
        subject.post_off(0.1) do |v|
          sleep(0.2)
          ex = true
        end
        sleep 0.1
        subject.await
        expect(timeout).to eq false
        sleep 0.3
        expect(timeout).to eq false
        expect(ex).to eq true
      end
    end

    context 'behavior' do

      # dereferenceable

      def dereferenceable_subject(value, opts = {})
        opts = opts.merge(executor: Concurrent::ImmediateExecutor.new)
        Agent.new(value, opts)
      end

      def dereferenceable_observable(opts = {})
        opts = opts.merge(executor: Concurrent::ImmediateExecutor.new)
        Agent.new(0, opts)
      end

      def execute_dereferenceable(subject)
        subject.post { |value| 10 }
        sleep(0.1)
      end

      it_should_behave_like :dereferenceable

      # observable

      subject { Agent.new(0) }

      def trigger_observable(observable)
        observable.post { nil }
        sleep(0.1)
      end

      it_should_behave_like :observable
    end

    context '#initialize' do

      let(:executor) { ImmediateExecutor.new }

      it 'sets the value to the given initial state' do
        expect(Agent.new(10).value).to eq 10
      end

      it 'uses the executor given with the :executor option' do
        expect(executor).to receive(:post).with(any_args).and_return(0)
        agent = Agent.new(0, executor: executor)
        agent.post { |value| 0 }
      end

      it 'uses the global io executor when :executor is :io' do
        expect(Concurrent).to \
          receive(:global_io_executor).at_least(:once).and_return(executor)
        agent = Agent.new(0, executor: :io)
        agent.post { |value| 0 }
      end

      it 'uses the global fast executor when :executor is :fast' do
        expect(Concurrent).to \
          receive(:global_fast_executor).at_least(:once).and_return(executor)
        agent = Agent.new(0, executor: :fast)
        agent.post { |value| 0 }
      end

      it 'uses the global io executor for #post by default' do
        expect(Concurrent).to \
          receive(:global_io_executor).at_least(:once).and_return(executor)
        agent = Agent.new(0)
        agent.post { |value| 0 }
      end

      it 'uses the global io executor for #post_off by default' do
        expect(Concurrent).to \
          receive(:global_io_executor).at_least(:once).and_return(executor)
        agent = Agent.new(0)
        agent.post_off { |value| 0 }
      end
    end

    context '#rescue' do

      it 'returns self when a block is given' do
        a1 = subject
        a2 = a1.rescue {}

        expect(a2).to be a1
      end

      it 'returns self when no block is given' do
        a1 = subject
        a2 = a1.rescue

        expect(a2).to be a1
      end

      it 'accepts an exception class as the first parameter' do
        expect {
          subject.rescue(StandardError) {}
        }.not_to raise_error
      end

      it 'ignores rescuers without a block' do
        subject.rescue
        expect(subject.instance_variable_get(:@rescuers)).to be_empty
      end
    end

    context '#validate' do

      it 'returns self when a block is given' do
        a1 = subject
        a2 = a1.validate {}

        expect(a2).to be a1
      end

      it 'returns self when no block is given' do
        a1 = subject
        a2 = a1.validate

        expect(a2).to be a1
      end

      it 'ignores validators without a block' do
        default_validator = subject.instance_variable_get(:@validator)
        subject.validate
        expect(subject.instance_variable_get(:@validator)).to be default_validator
      end
    end

    context '#post' do

      it 'adds the given block to the queue' do
        expect(executor).to receive(:post).with(no_args).exactly(1).times
        subject.post { sleep(1) }
        subject.post { nil }
        subject.post { nil }
        sleep(0.1)
        expect(subject.
               instance_variable_get(:@serialized_execution).
               instance_variable_get(:@stash).
               size).to eq 2
      end

      it 'does not add to the queue when no block is given' do
        expect(executor).to receive(:post).with(no_args).exactly(0).times
        subject.post
        sleep(0.1)
      end

      it 'works with ImmediateExecutor' do
        agent = Agent.new(0, executor: ImmediateExecutor.new)
        agent.post { |old| old + 1 }
        agent.post { |old| old + 1 }
        expect(agent.value).to eq 2
      end

    end

    context '#await' do

      it 'waits until already sent updates are done' do
        actual = Concurrent::AtomicBoolean.new(false)
        latch = Concurrent::CountDownLatch.new
        subject.post { latch.count_down; sleep(0.1); actual.make_true }
        latch.wait(1)
        subject.await
        expect(actual.value).to be true
      end

      it 'does not alter the value' do
        subject.post { |v| v + 1 }
        subject.await
        expect(subject.value).to eq 1
      end

    end

    context 'fulfillment' do

      it 'process each block in the queue' do
        latch = Concurrent::CountDownLatch.new(3)
        subject.post { latch.count_down }
        subject.post { latch.count_down }
        subject.post { latch.count_down }
        expect(latch.wait(1)).to be_truthy
      end

      it 'passes the current value to the handler' do
        latch = Concurrent::CountDownLatch.new(5)
        Agent.new(latch.count, executor: executor).post do |i|
          i.times { latch.count_down }
        end
        expect(latch.wait(1)).to be_truthy
      end

      it 'sets the value to the handler return value on success' do
        agent = Agent.new(10, executor: Concurrent::ImmediateExecutor.new)
        expect(agent.value).to eq 10
        agent.post { 100 }
        expect(agent.value).to eq 100
      end

      it 'rejects the handler after timeout reached' do
        agent = Agent.new(0, timeout: 0.1, executor: executor)
        agent.post { sleep(1); 10 }
        sleep(0.2)
        expect(agent.value).to eq 0
      end
    end

    context 'validation' do

      it 'processes the validator when present' do
        latch = Concurrent::CountDownLatch.new(1)
        subject.validate { latch.count_down; true }
        subject.post { nil }
        expect(latch.wait(1)).to be_truthy
      end

      it 'passes the new value to the validator' do
        expected = Concurrent::AtomicFixnum.new(0)
        latch    = Concurrent::CountDownLatch.new(1)
        subject.validate { |v| expected.value = v; latch.count_down; true }
        subject.post { 10 }
        latch.wait(1)
        expect(expected.value).to eq 10
      end

      it 'sets the new value when the validator returns true' do
        agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).validate { true }
        agent.post { 10 }
        expect(agent.value).to eq 10
      end

      it 'does not change the value when the validator returns false' do
        agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).validate { false }
        agent.post { 10 }
        expect(agent.value).to eq 0
      end

      it 'does not change the value when the validator raises an exception' do
        agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).validate { raise StandardError }
        agent.post { 10 }
        expect(agent.value).to eq 0
      end
    end

    context 'rejection' do

      it 'calls the first exception block with a matching class' do
        expected = nil
        agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
      rescue(StandardError) { |ex| expected = 1 }.
        rescue(StandardError) { |ex| expected = 2 }.
        rescue(StandardError) { |ex| expected = 3 }
          agent.post { raise StandardError }
          expect(expected).to eq 1
        end

      it 'matches all with a rescue with no class given' do
        expected = nil
        agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
      rescue(LoadError) { |ex| expected = 1 }.
        rescue { |ex| expected = 2 }.
        rescue(StandardError) { |ex| expected = 3 }
          agent.post { raise NoMethodError }
          expect(expected).to eq 2
        end

      it 'searches associated rescue handlers in order' do
        expected = nil
        agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
      rescue(ArgumentError) { |ex| expected = 1 }.
        rescue(LoadError) { |ex| expected = 2 }.
        rescue(StandardError) { |ex| expected = 3 }
          agent.post { raise ArgumentError }
          expect(expected).to eq 1

          expected = nil
          agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
        rescue(ArgumentError) { |ex| expected = 1 }.
          rescue(LoadError) { |ex| expected = 2 }.
          rescue(StandardError) { |ex| expected = 3 }
            agent.post { raise LoadError }
            expect(expected).to eq 2

            expected = nil
            agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
          rescue(ArgumentError) { |ex| expected = 1 }.
            rescue(LoadError) { |ex| expected = 2 }.
            rescue(StandardError) { |ex| expected = 3 }
              agent.post { raise StandardError }
              expect(expected).to eq 3
            end

      it 'passes the exception object to the matched block' do
        expected = nil
        agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
      rescue(ArgumentError) { |ex| expected = ex }.
        rescue(LoadError) { |ex| expected = ex }.
        rescue(StandardError) { |ex| expected = ex }
          agent.post { raise StandardError }
          expect(expected).to be_a(StandardError)
        end

      it 'ignores rescuers without a block' do
        expected = nil
        agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
      rescue(StandardError).
        rescue(StandardError) { |ex| expected = ex }
          agent.post { raise StandardError }
          expect(expected).to be_a(StandardError)
        end

      it 'supresses the exception if no rescue matches' do
        expect {
          agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).
      rescue(ArgumentError) { |ex| @expected = ex }.
        rescue(NotImplementedError) { |ex| @expected = ex }.
        rescue(NoMethodError) { |ex| @expected = ex }
          agent.post { raise StandardError }
        }.not_to raise_error
        end

      it 'suppresses exceptions thrown from rescue handlers' do
        expect {
          agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new).rescue(StandardError) { raise StandardError }
          agent.post { raise ArgumentError }
        }.not_to raise_error
      end
    end

    context 'observation' do

      it 'notifies all observers when the value changes' do
        agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new)
        agent.add_observer(observer)
        agent.post { 10 }
        expect(observer.value).to eq 10
      end

      it 'does not notify removed observers when the value changes' do
        agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new)
        agent.add_observer(observer)
        agent.delete_observer(observer)
        agent.post { 10 }
        expect(observer.value).to be_nil
      end

      it 'does not notify observers when validation fails' do
        agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new)
        agent.validate { false }
        agent.add_observer(observer)
        agent.post { 10 }
        expect(observer.value).to be_nil
      end

      it 'does not notify observers when the handler raises an exception' do
        agent = Agent.new(0, executor: Concurrent::ImmediateExecutor.new)
        agent.add_observer(observer)
        agent.post { raise StandardError }
        expect(observer.value).to be_nil
      end
    end

    context 'clojure-like behaviour' do
      it 'does not block dereferencing when updating the value' do
        continue = IVar.new
        agent    = Agent.new(0, executor: executor)
        agent.post { |old| old + continue.value }
        sleep 0.1
        expect(agent.value).to eq 0
        continue.set 1
        agent.await
        expect(agent.value).to eq 1
        sleep 0.1
      end

      it 'does not allow to execute two updates at the same time' do
        agent     = Agent.new(0, executor: executor)
        continue1 = IVar.new
        continue2 = IVar.new
        f1        = f2 = false
        agent.post { |old| f1 = true; old + continue1.value }
        agent.post { |old| f2 = true; old + continue2.value }

        sleep 0.1
        expect(f1).to eq true
        expect(f2).to eq false
        expect(agent.value).to eq 0

        continue1.set 1
        sleep 0.1
        expect(f1).to eq true
        expect(f2).to eq true
        expect(agent.value).to eq 1

        continue2.set 1
        sleep 0.1
        expect(f1).to eq true
        expect(f2).to eq true
        expect(agent.value).to eq 2
      end

      it 'waits with sending functions to other agents until update is done'
    end

    context 'aliases' do

      it 'aliases #deref for #value' do
        expect(Agent.new(10, executor: executor).deref).to eq 10
      end

      it 'aliases #validates for :validate' do
        latch = Concurrent::CountDownLatch.new(1)
        subject.validates { latch.count_down; true }
        subject.post { nil }
        expect(latch.wait(1)).to be_truthy
      end

      it 'aliases #validate_with for :validate' do
        latch = Concurrent::CountDownLatch.new(1)
        subject.validate_with { latch.count_down; true }
        subject.post { nil }
        expect(latch.wait(1)).to be_truthy
      end

      it 'aliases #validates_with for :validate' do
        latch = Concurrent::CountDownLatch.new(1)
        subject.validates_with { latch.count_down; true }
        subject.post { nil }
        expect(latch.wait(1)).to be_truthy
      end

      it 'aliases #catch for #rescue' do
        agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new)
        expected = nil
        agent.catch { expected = true }
        agent.post { raise StandardError }
        expect(agent).to be_truthy
      end

      it 'aliases #on_error for #rescue' do
        agent    = Agent.new(0, executor: Concurrent::ImmediateExecutor.new)
        expected = nil
        agent.on_error { expected = true }
        agent.post { raise StandardError }
        expect(agent).to be_truthy
      end
    end
  end
end
