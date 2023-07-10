require 'concurrent/channel'
require 'concurrent/atomic/count_down_latch'
require 'concurrent/executor/immediate_executor'

module Concurrent

 RSpec.describe Channel, edge: true do

    context 'initialization' do

      it 'raises an exception when the :buffer is invalid' do
        expect {
          Channel.new(buffer: :bogus)
        }.to raise_error(ArgumentError)
      end

      it 'is :unbuffered when neither :buffer nore :capacity is given' do
        expect(Channel::Buffer::Unbuffered).to receive(:new).with(no_args).and_call_original
        Channel.new
      end

      it 'is :unbuffered when :unbuffered is given' do
        expect(Channel::Buffer::Unbuffered).to receive(:new).with(no_args).and_call_original
        Channel.new(buffer: :unbuffered)
      end

      it 'is :unbuffered when :buffered and capacity: 0' do
        expect(Channel::Buffer::Unbuffered).to receive(:new).with(no_args).and_call_original
        Channel.new(buffer: :buffered, capacity: 0)
      end

      it 'raises an exception when both :unbuffered and :capacity are given' do
        expect {
          Channel.new(buffer: :unbuffered, capacity: 0)
        }.to raise_error(ArgumentError)
      end

      it 'is :buffered when :capacity > 0 and no :buffer given' do
        expect(Channel::Buffer::Buffered).to receive(:new).with(5).and_call_original
        Channel.new(capacity: 5)
      end

      it 'is :buffered when :buffered given' do
        expect(Channel::Buffer::Buffered).to receive(:new).with(5).and_call_original
        Channel.new(buffer: :buffered, capacity: 5)
      end

      it 'raises an exception when :buffered given without :capacity' do
        expect {
          Channel.new(buffer: :buffered)
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception when :buffered and :capacity < 0' do
        expect {
          Channel.new(buffer: :buffered, capacity: -1)
        }.to raise_error(ArgumentError)
      end

      it 'is :dropping when :dropping and :capacity > 0' do
        expect(Channel::Buffer::Dropping).to receive(:new).with(5).and_call_original
        Channel.new(buffer: :dropping, capacity: 5)
      end

      it 'raises an exception when :dropping given without :capacity' do
        expect {
          Channel.new(buffer: :dropping)
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception when :dropping and :capacity < 1' do
        expect {
          Channel.new(buffer: :dropping, capacity: 0)
        }.to raise_error(ArgumentError)
      end

      it 'is :sliding when :sliding and :capacity > 0' do
        expect(Channel::Buffer::Sliding).to receive(:new).with(5).and_call_original
        Channel.new(buffer: :sliding, capacity: 5)
      end

      it 'raises an exception when :sliding given without :capacity' do
        expect {
          Channel.new(buffer: :sliding)
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception when :sliding and :capacity < 1' do
        expect {
          Channel.new(buffer: :sliding, capacity: 0)
        }.to raise_error(ArgumentError)
      end

      it 'uses the given buffer' do
        buffer = Channel::Buffer::Buffered.new(10)
        subject = Channel.new(buffer)
        expect(subject).to receive(:put).with(42)
        subject.put(42)
      end
    end

    context 'factories' do

      specify do
        expect(Channel::Buffer::Ticker).to receive(:new).with(10).and_call_original
        Channel.ticker(10)
      end

      specify do
        expect(Channel::Buffer::Timer).to receive(:new).with(10).and_call_original
        Channel.timer(10)
      end
    end

    context '#put' do

      it 'returns true on success' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        expect(subject.put(:foo)).to be true
      end

      it 'returns false on failure' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        subject.close
        expect(subject.put(:foo)).to be false
      end

      it 'rejects when the validator returns false' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.put(42)).to be false
      end

      it 'rejects when the validator raises an exception' do
        validator = ->(value) { raise StandardError }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.put(42)).to be false
      end

      it 'rejects nil' do
        expect(subject.put(nil)).to be false
      end
    end

    context 'put!' do

      it 'returns true on success' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        expect(subject.put!(:foo)).to be true
      end

      it 'raises an exception on failure' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        subject.close
        expect {
          subject.put!(:foo)
        }.to raise_error(Channel::Error)
      end

      it 'rejects when the validator returns false' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect{
          subject.put!(42)
        }.to raise_error(Channel::ValidationError)
      end

      it 'rejects when the validator raises an exception' do
        validator = ->(value) { raise StandardError }
        subject = Channel.new(capacity: 10, validator: validator)
        expect{
          subject.put!(42)
        }.to raise_error(StandardError)
      end

      it 'rejects nil' do
        expect {
          subject.put!(nil)
        }.to raise_error(Channel::ValidationError)
      end
    end

    context 'put?' do

      it 'returns a just Maybe on success' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        result = subject.put?(:foo)
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_just
      end

      it 'returns a nothing Maybe on failure' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        subject.close
        result = subject.put?(:foo)
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_nothing
      end

      it 'rejects when the validator returns false' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.put?(42)).to be_nothing
      end

      it 'rejects when the validator raises an exception' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.put?(42)).to be_nothing
      end

      it 'accepts nil' do
        result = subject.put?(nil)
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_just
      end
    end

    context '#offer' do

      it 'returns true on success' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        expect(subject.offer(:foo)).to be true
      end

      it 'returns false on failure' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        subject.close
        expect(subject.offer(:foo)).to be false
      end

      it 'rejects when the validator returns false' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.offer(42)).to be false
      end

      it 'rejects when the validator raises an exception' do
        validator = ->(value) { raise StandardError }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.offer(42)).to be false
      end

      it 'rejects nil' do
        expect(subject.offer(nil)).to be false
      end
    end

    context 'offer!' do

      it 'returns true on success' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        expect(subject.offer!(:foo)).to be true
      end

      it 'raises an exception on failure' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        subject.close
        expect {
          subject.offer!(:foo)
        }.to raise_error(Channel::Error)
      end

      it 'rejects when the validator returns false' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect{
          subject.offer!(42)
        }.to raise_error(Channel::ValidationError)
      end

      it 'rejects when the validator raises an exception' do
        validator = ->(value) { raise StandardError }
        subject = Channel.new(capacity: 10, validator: validator)
        expect{
          subject.offer!(42)
        }.to raise_error(StandardError)
      end

      it 'rejects nil' do
        expect {
          subject.offer!(nil)
        }.to raise_error(Channel::ValidationError)
      end
    end

    context 'offer?' do

      it 'returns a just Maybe on success' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        result = subject.offer?(:foo)
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_just
      end

      it 'returns a nothing Maybe on failure' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        subject.close
        result = subject.offer?(:foo)
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_nothing
      end

      it 'rejects when the validator returns false' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.offer?(42)).to be_nothing
      end

      it 'rejects when the validator raises an exception' do
        validator = ->(value) { false }
        subject = Channel.new(capacity: 10, validator: validator)
        expect(subject.offer?(42)).to be_nothing
      end

      it 'accepts nil' do
        subject = Channel.new(buffer: :buffered, capacity: 2)
        result = subject.offer?(nil)
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_just
      end
    end

    context '#take' do

      subject { Channel.new(buffer: :buffered, capacity: 2) }

      it 'takes the next item when not empty' do
        subject.put(:foo)
        expect(subject.take).to eq :foo
      end

      it 'returns nil on failure' do
        subject.close
        expect(subject.take).to be nil
      end
    end

    context '#take!' do

      subject { Channel.new(buffer: :buffered, capacity: 2) }

      it 'takes the next item when not empty' do
        subject.put(:foo)
        expect(subject.take!).to eq :foo
      end

      it 'raises an exception on failure' do
        subject.close
        expect {
          subject.take!
        }.to raise_error(Channel::Error)
      end
    end

    context '#take?' do

      subject { Channel.new(buffer: :buffered, capacity: 2) }

      it 'returns a just Maybe on success' do
        subject.put(:foo)
        result = subject.take?
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_just
        expect(result.value).to eq :foo
      end

      it 'returns a nothing Maybe on failure' do
        subject.close
        result = subject.take?
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_nothing
      end
    end

    context '#next' do

      subject { Channel.new(buffer: :buffered, capacity: 3) }

      it 'returns <item>, true when there is one item' do
        subject.put(:foo)
        item, more = subject.next
        expect(item).to eq :foo
        expect(more).to be true
      end

      it 'returns <item>, true when there are multiple items' do
        subject.put(:foo)
        subject.put(:bar)
        item, more = subject.next
        subject.poll # clear the buffer

        expect(item).to eq :foo
        expect(more).to be true
      end

      it 'returns nil, false when empty and closed' do
        subject.close
        item, more = subject.next
        expect(item).to be nil
        expect(more).to be false
      end

      it 'returns <item>, true when closed and last item' do
        capacity = subject.capacity
        expect(capacity).to be >= 1

        capacity.times { subject.put(:foo) }
        subject.close

        capacity.times do
          item, more = subject.next
          expect(item).to eq :foo
          expect(more).to be true
        end
      end

      it 'returns nil, false when closed and no items remain' do
        capacity = subject.capacity
        expect(capacity).to be >= 1

        capacity.times { subject.put(:foo) }
        subject.close

        capacity.times { subject.next }

        item, more = subject.next
        expect(item).to be_nil
        expect(more).to be false
      end
    end

    context '#next?' do

      subject { Channel.new(buffer: :buffered, capacity: 2) }

      it 'returns a just Maybe and true when there is one item' do
        subject.put(:foo)
        item, more = subject.next?
        expect(item).to be_a Concurrent::Maybe
        expect(item).to be_just
        expect(item.value).to eq :foo
        expect(more).to be true
      end

      it 'returns a just Maybe, true when there are multiple items' do
        subject.put(:foo)
        subject.put(:bar)
        item, more = subject.next?
        subject.poll # clear the buffer

        expect(item).to be_a Concurrent::Maybe
        expect(item).to be_just
        expect(item.value).to eq :foo
        expect(more).to be true
      end

      it 'returns a nothing Maybe and false on failure' do
        subject.close
        item, more = subject.next?
        expect(item).to be_a Concurrent::Maybe
        expect(item).to be_nothing
        expect(more).to be false
      end
    end

    context '#poll' do

      it 'returns the next item immediately if available' do
        subject # initialize on this thread
        t = in_thread do
          subject.put(42)
        end
        t.join(0.1)

        expect(subject.poll).to eq 42
      end

      it 'returns nil immediately if no item is available' do
        expect(subject.poll).to be nil
      end

      it 'returns nil on failure' do
        subject.close
        expect(subject.poll).to be nil
      end
    end

    context '#poll!' do

      it 'returns the next item immediately if available' do
        subject # initialize on this thread
        t = in_thread do
          subject.put(42)
        end
        t.join(0.1)

        expect(subject.poll!).to eq 42
      end

      it 'raises an exception immediately if no item is available' do
        expect {
          subject.poll!
        }.to raise_error(Channel::Error)
      end

      it 'raises an exception on failure' do
        subject.close
        expect {
          subject.poll!
        }.to raise_error(Channel::Error)
      end
    end

    context '#poll?' do

      it 'returns a just Maybe immediately if available' do
        subject # initialize on this thread
        t = in_thread do
          subject.put(42)
        end
        t.join(0.1)

        result = subject.poll?
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_just
        expect(result.value).to eq 42
      end

      it 'returns a nothing Maybe immediately if no item is available' do
        result = subject.poll?
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_nothing
      end

      it 'returns a nothing Maybe on failure' do
        subject.close
        result = subject.poll?
        expect(result).to be_a Concurrent::Maybe
        expect(result).to be_nothing
      end
    end

    context '.each' do

      it 'raises and exception when no block is given' do
        expect {
          subject.each
        }.to raise_error(ArgumentError)
      end

      it 'iterates until the channel is closed' do
        expected = [13, 42, 2001]
        subject = Channel.new(capacity: expected.length)
        expected.each { |value| subject.put(value) }
        subject.close

        actual = []
        subject.each { |value| actual << value }
        expect(actual).to eq expected
      end
    end

    context 'goroutines' do

      let(:default_executor) { Channel.const_get(:GOROUTINES) }

      context '.go' do

        it 'raises an exception when no block is given' do
          expect {
            Channel.go
          }.to raise_error(ArgumentError)
        end

        specify do
          expect(default_executor).to receive(:post).with(1, 2, 3)
          Channel.go(1, 2, 3) { nil }
        end
      end

      context '.go_via' do

        it 'raises an exception when no block is given' do
          expect {
            Channel.go_via
          }.to raise_error(ArgumentError)
        end

        specify do
          executor = ImmediateExecutor.new
          expect(executor).to receive(:post).with(1, 2, 3)
          Channel.go_via(executor, 1, 2, 3) { nil }
        end
      end

      context '.go_loop' do

        it 'raises an exception when no block is given' do
          expect {
            Channel.go_loop
          }.to raise_error(ArgumentError)
        end

        it 'loops until the block returns false' do
          actual = 0
          expected = 3
          latch = Concurrent::CountDownLatch.new(expected)
          Channel.go_loop do
            actual += 1
            latch.count_down
            actual < expected
          end

          latch.wait(10)
          expect(actual).to eq expected
        end
      end

      context '.go_loop_via' do

        it 'raises an exception when no block is given' do
          expect {
            Channel.go_loop_via
          }.to raise_error(ArgumentError)
        end

        it 'loops until the block returns false' do
          actual = 0
          expected = 3
          executor = ImmediateExecutor.new
          latch = Concurrent::CountDownLatch.new(expected)
          Channel.go_loop_via(executor) do
            actual += 1
            latch.count_down
            actual < expected
          end

          latch.wait(3)
          expect(actual).to eq expected
        end
      end
    end

    context 'select' do

      it 'raises an exception when no block is given' do
        expect {
          Channel.select
        }.to raise_error(ArgumentError)
      end

      it 'passes a selector to the block' do
        actual = nil
        Channel.select { |s| actual = s; s.error {  } }
        expect(actual).to be_a Channel::Selector
      end

      specify do
        expect_any_instance_of(Channel::Selector).to receive(:execute)
        Channel.select { |s| s.error {  } }
      end
    end
 end
end
