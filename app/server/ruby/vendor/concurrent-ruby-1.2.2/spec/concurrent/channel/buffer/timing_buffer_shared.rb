require_relative 'base_shared'
require 'concurrent/atomic/atomic_boolean'

RSpec.shared_examples :channel_timing_buffer do

  specify do
    expect(subject).to be_blocking
  end

  context '#capacity' do
    specify do
      expect(subject.capacity).to eq 1
    end
  end

  context '#size' do
    specify do
      expect(subject.size).to eq 0
    end
  end

  context '#empty?' do
    specify do
      expect(subject).to_not be_empty
    end
  end

  context '#full?' do
    specify do
      expect(subject).to be_full
    end
  end

  context '#put' do
    specify do
      expect(subject.put(:foo)).to be false
    end
  end

  context '#offer' do
    specify do
      expect(subject.offer(:foo)).to be false
    end
  end

  context '#take' do

    it 'blocks when the timer is not ready' do
      actual = Concurrent::AtomicBoolean.new(false)
      subject = described_class.new(10)
      t = in_thread do
        subject.take
        actual.make_true
      end
      t.join(0.1)
      actual = actual.value
      t.kill # clean up
      expect(actual).to be false
    end

    it 'returns a Tick' do
      subject = described_class.new(0.1)
      expect(subject.take).to be_a Concurrent::Channel::Tick
    end

    it 'triggers after the specified time interval' do
      start = Concurrent::Channel::Tick.new.monotonic
      subject = described_class.new(0.1)
      actual = subject.take.monotonic
      expect(actual - start).to be >= 0.1
    end

    it 'returns Concurrent::NULL when closed' do
      subject.close
      expect(subject.take).to eq Concurrent::NULL
    end
  end

  context '#poll' do

    it 'returns Concurrent::NULL when the timer is not ready' do
      subject = described_class.new(0.1)
      expect(subject.poll).to eq Concurrent::NULL
    end

    it 'returns a Tick' do
      subject = described_class.new(0.1)
      sleep(0.2)
      expect(subject.poll).to be_a Concurrent::Channel::Tick
    end

    it 'returns Concurrent::NULL when closed' do
      subject.close
      expect(subject.poll).to eq Concurrent::NULL
    end

    it 'triggers after the specified time interval' do
      start = Concurrent::Channel::Tick.new.monotonic
      subject = described_class.new(0.1)
      sleep(0.2)
      actual = subject.poll.monotonic
      expect(actual - start).to be >= 0.1
    end
  end

  context '#next' do

    it 'blocks when the timer is not ready' do
      actual = Concurrent::AtomicBoolean.new(false)
      subject = described_class.new(10)
      t = in_thread do
        subject.next
        actual.make_true
      end
      t.join(0.1)
      actual = actual.value
      t.kill # clean up
      expect(actual).to be false
    end

    it 'returns a Tick when open' do
      subject = described_class.new(0.1)
      value, _ = subject.next
      expect(value).to be_a Concurrent::Channel::Tick
    end

    it 'returns Concurrent::NULL, false when closed' do
      subject.close
      expect(subject.take).to eq Concurrent::NULL
    end

    it 'triggers after the specified time interval' do
      start = Concurrent::Channel::Tick.new.monotonic
      subject = described_class.new(0.1)
      actual, _ = subject.next
      expect(actual.monotonic - start).to be >= 0.1
    end
  end

  context '#close' do

    it 'sets #closed? to false' do
      subject.close
      expect(subject).to be_closed
    end

    it 'returns true when not previously closed' do
      expect(subject.close).to be true
    end

    it 'returns false when already closed' do
      subject.close
      expect(subject.close).to be false
    end
  end

  context '#closed?' do

    it 'returns true when new' do
      expect(subject).to_not be_closed
    end

    it 'returns false after #close' do
      subject.close
      expect(subject).to be_closed
    end
  end
end
