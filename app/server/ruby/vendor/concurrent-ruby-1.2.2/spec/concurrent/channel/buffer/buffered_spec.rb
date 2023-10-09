require_relative 'buffered_shared'

module Concurrent::Channel::Buffer

  RSpec.describe Buffered, edge: true do

    let(:capacity) { 10 }
    subject { described_class.new(capacity) }

    it_behaves_like :channel_buffered_buffer

    specify do
      expect(subject).to be_blocking
    end

    context '#full?' do
      it 'returns true when at max capacity' do
        subject = described_class.new(1)
        subject.put(:foo)
        expect(subject).to be_full
      end
    end

    context '#put' do
      it 'blocks when at capacity until a thread is ready to take' do
        subject = described_class.new(1)
        subject.put(13)
        bucket = Concurrent::AtomicReference.new(nil)
        t = in_thread do
          subject.put(42)
          bucket.value = 42
        end

        t.join(0.1)

        before = bucket.value
        subject.take
        t.join(0.1)
        after = bucket.value

        expect(before).to be nil
        expect(after).to eq 42
        expect(t.status).to be false
      end
    end

    context '#offer' do
      it 'returns false immediately when full' do
        subject = described_class.new(1)
        subject.put(:foo)
        expect(subject.offer(:bar)).to be false
      end
    end
  end
end
