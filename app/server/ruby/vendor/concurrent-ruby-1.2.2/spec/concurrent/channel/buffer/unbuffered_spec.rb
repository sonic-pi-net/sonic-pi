require_relative 'base_shared'
require 'concurrent/channel/buffer/unbuffered'

module Concurrent::Channel::Buffer

  RSpec.describe Unbuffered, edge: true do

    subject { described_class.new }

    it_behaves_like :channel_buffer

    specify do
      expect(subject).to be_blocking
    end

    specify do
      expect(subject.capacity).to eq 1
    end

    context '#size' do

      it 'is 0 when first created' do
        expect(subject.size).to eq 0
      end

      it 'is 1 when a putting thread is waiting' do
        t = in_thread { subject.put(:foo) }
        t.join(0.1)
        expect(subject.size).to eq 1
        t.kill # cleanup
      end

      it 'is 0 when there are taking threads but no putting threads' do
        t = in_thread { subject.take }
        t.join(0.1)
        expect(subject.size).to eq 0
        t.kill # cleanup
      end
    end

    context '#empty?' do

      it 'is true when there are no putting threads' do
        expect(subject).to be_empty
      end

      it 'is false when there are waiting putting threads' do
        t = in_thread { subject.put(:foo) }
        t.join(0.1)
        expect(subject).to_not be_empty
        t.kill # cleanup
      end
    end

    context '#full?' do

      it 'is false when there are no putting threads' do
        expect(subject).to_not be_full
      end

      it 'is false when there are waiting putting threads' do
        t = in_thread { subject.put(:foo) }
        t.join(0.1)
        expect(subject).to be_full
        t.kill # cleanup
      end
    end

    context '#put' do

      it 'does not enqueue the item when closed' do
        subject.close
        subject.put(:foo)
        expect(subject).to be_empty
      end

      it 'returns false when closed' do
        subject.close
        expect(subject.put(:foo)).to be false
      end

      it 'blocks until a thread is ready to take' do
        subject # initialize on this thread
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

      it 'delivers when closed after put starts' do
        t = in_thread do
          subject.put(:foo)
        end
        t.join(0.1)
        subject.close

        item = subject.take
        t.kill #clean up

        expect(item).to eq :foo
      end
    end

    context '#offer' do

      it 'returns false immediately when a put in in progress' do
        subject # initialize on this thread
        t = in_thread do
          subject.put(:foo) # block the thread
        end
        t.join(0.1)

        ok = subject.offer(:bar)
        subject.poll # release the blocked thread

        expect(ok).to be false
      end

      it 'gives the item to a waiting taker and returns true' do
        subject # initialize on this thread
        bucket = Concurrent::AtomicReference.new(nil)
        t = in_thread do
          bucket.value = subject.take
        end
        t.join(0.1)

        before = bucket.value
        ok = subject.offer(42)
        t.join(0.1)
        after = bucket.value

        expect(ok).to be true
        expect(before).to be nil
        expect(after).to eq 42
      end
    end

    context '#take' do

      it 'returns false immediately when a put in in progress' do
        subject # initialize on this thread
        t = in_thread do
          subject.put(:foo) # block the thread
        end
        t.join(0.1)

        ok = subject.offer(:bar)
        subject.poll # release the blocked thread

        expect(ok).to be false
      end

      it 'gives the item to a waiting taker and returns true' do
        subject # initialize on this thread
        bucket = Concurrent::AtomicReference.new(nil)
        t = in_thread do
          bucket.value = subject.take
        end
        t.join(0.1)

        before = bucket.value
        ok = subject.offer(42)
        t.join(0.1)
        after = bucket.value

        expect(ok).to be true
        expect(before).to be nil
        expect(after).to eq 42
      end
    end

    context '#next' do

      it 'blocks when no putting and returns <item>, true when one arrives' do
        subject # initialize on this thread
        bucket = Concurrent::AtomicReference.new([])
        t = in_thread do
          bucket.value = subject.next
        end
        t.join(0.1)

        before = bucket.value
        subject.put(42)
        t.join(0.1)
        after = bucket.value

        expect(before).to eq []
        expect(after.first).to eq 42
        expect(after.last).to be true
        expect(t.status).to be false
      end

      it 'returns <item>, true when there are multiple putting' do
        subject # initialize on this thread
        threads = 2.times.collect do
          in_thread do
            subject.put(42)
          end
        end
        threads.each {|t| t.join(0.1)}

        item, more = subject.next
        subject.poll # clear the channel

        expect(item).to eq 42
        expect(more).to be true
      end

      it 'returns <item>, true when closed and last item' do
        t = in_thread do
          subject.put(:foo)
        end
        t.join(0.1)
        subject.close

        item, more = subject.next
        t.kill #clean up

        expect(item).to eq :foo
        expect(more).to be true
      end

      it 'returns Concurrent::NULL, false when closed and no items remain' do
        skip('flaky on truffleruby') if Concurrent.on_truffleruby?

        t = in_thread do
          subject.put(:foo)
        end
        subject.close

        subject.next
        item, more = subject.next
        t.kill #clean up

        expect(item).to eq Concurrent::NULL
        expect(more).to be false
      end
    end
  end
end
