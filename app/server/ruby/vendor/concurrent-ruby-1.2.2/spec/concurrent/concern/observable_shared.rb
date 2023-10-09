require 'concurrent/atomic/count_down_latch'

RSpec.shared_examples :observable do

  let(:observer_set) do
    subject.instance_variable_get(:@observers)
  end

  let(:observer_class) do
    Class.new do
      def initialize(&block)
        @block = block
      end
      def update(*args)
        @block.call(*args) if @block
      end
    end
  end

  let(:observer){ observer_class.new }

  let!(:observer_func){ :notify }

  let(:observer_with_func_class) do
    Class.new do
      def initialize(&block)
        @block = block
      end
      def notify(*args)
        @block.call(*args) if @block
      end
    end
  end

  let(:observer_with_func){ observer_with_func_class.new }

  context '#add_observer' do

    it 'adds an observer if called before first notification' do
      expect(observer_set).to receive(:add_observer).with(any_args)
      subject.add_observer(observer)
    end

    it 'adds an observer with :func if called before first notification' do
      expect(observer_set).to receive(:add_observer).with(observer_with_func, :notify)
      subject.add_observer(observer_with_func, observer_func)
    end

    it 'creates an observer from a block if called before first notification' do
      block = proc{ nil }
      expect(observer_set).to receive(:add_observer).with(any_args)
      subject.add_observer(&block)
    end

    it 'raises an exception if not given an observer or a block' do
      expect {
        subject.add_observer
      }.to raise_error(ArgumentError)
    end

    it 'raises an exception when given both an observer and a block' do
      expect {
        subject.add_observer(observer){ nil }
      }.to raise_error(ArgumentError)
    end
  end

  context '#delete_observer' do

    it 'deletes the given observer if called before first notification' do
      expect(subject.count_observers).to eq 0
      subject.add_observer(observer)
      expect(subject.count_observers).to eq 1
      subject.delete_observer(observer)
      expect(subject.count_observers).to eq 0
    end

    it 'returns the removed observer if found in the observer set' do
      subject.add_observer(observer)
      expect(subject.delete_observer(observer)).to eq observer
    end

    it 'returns the given observer even when not found in the observer set' do
      expect(subject.delete_observer(observer)).to eq observer
    end
  end

  context '#delete_observers' do

    it 'deletes all observers when called before first notification' do
      5.times{ subject.add_observer(observer_class.new) }
      expect(subject.count_observers).to eq 5
      subject.delete_observers
      expect(subject.count_observers).to eq 0
    end

    it 'returns self' do
      expect(subject.delete_observers).to eq subject
    end
  end

  context '#count_observers' do

    it 'returns zero for a new observable object' do
      expect(subject.count_observers).to eq 0
    end

    it 'returns a count of registered observers if called before first notification' do
      5.times{ subject.add_observer(observer_class.new) }
      expect(subject.count_observers).to eq 5
    end

    it 'returns zero after #delete_observers has been called' do
      5.times{ subject.add_observer(observer_class.new) }
      subject.delete_observers
      expect(subject.count_observers).to eq 0
    end
  end

  context 'first notification' do
    it 'calls the #update method on all observers without a specified :func' do
      latch = Concurrent::CountDownLatch.new(5)
      5.times do
        subject.add_observer(observer_class.new{ latch.count_down })
      end
      trigger_observable(subject)
      latch.wait(1)
      expect(latch.count).to eq 0
    end

    it 'calls the appropriate function on all observers which specified a :func' do
      latch = Concurrent::CountDownLatch.new(5)
      5.times do
        obs = observer_with_func_class.new{ latch.count_down }
        subject.add_observer(obs, observer_func)
      end
      trigger_observable(subject)
      latch.wait(1)
      expect(latch.count).to eq 0
    end

    it 'calls the proc for all observers added as a block' do
      latch = Concurrent::CountDownLatch.new(5)
      5.times do
        subject.add_observer{ latch.count_down }
      end
      trigger_observable(subject)
      latch.wait(1)
      expect(latch.count).to eq 0
    end

    it 'does not notify any observers removed with #delete_observer' do
      latch = Concurrent::CountDownLatch.new(5)

      obs = observer_class.new{ latch.count_down }
      subject.add_observer(obs)
      subject.delete_observer(obs)

      trigger_observable(subject)
      latch.wait(1)
      expect(latch.count).to eq 5
    end

    it 'does not notify any observers after #delete_observers called' do
      latch = Concurrent::CountDownLatch.new(5)
      5.times do
        subject.add_observer(observer_class.new{ latch.count_down })
      end

      subject.delete_observers

      trigger_observable(subject)
      latch.wait(1)
      expect(latch.count).to eq 5
    end
  end
end
