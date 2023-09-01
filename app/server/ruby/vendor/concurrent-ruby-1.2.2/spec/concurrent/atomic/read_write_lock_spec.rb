require 'concurrent/atomic/read_write_lock'
require 'concurrent/atomic/count_down_latch'
require 'concurrent/atomic/atomic_boolean'

module Concurrent

  RSpec.describe ReadWriteLock do

    context '#write_locked?' do

      it 'returns true when the write lock is held' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)

        in_thread do
          subject.with_write_lock do
            latch_1.count_down
            latch_2.wait(1)
          end
        end

        latch_1.wait(1)
        expect(subject).to be_write_locked
        latch_2.count_down
      end

      it 'returns false when the write lock is not held' do
        expect(subject).to_not be_write_locked
      end

      it 'returns false when the write lock is not held but there are readers' do
        latch = Concurrent::CountDownLatch.new(1)

        in_thread do
          subject.with_read_lock do
            latch.wait(1)
          end
        end

        expect(subject).to_not be_write_locked
        latch.count_down
      end
    end

    context '#has_waiters?' do

      it 'returns false when no locks are held' do
        expect(subject).to_not have_waiters
      end

      it 'returns false when there are readers but no writers' do
        latch = Concurrent::CountDownLatch.new(1)

        in_thread do
          subject.with_read_lock do
            latch.wait(1)
          end
        end

        expect(subject).to_not have_waiters
        latch.count_down
      end

      it 'returns true when the write lock is held and there are waiting readers' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        latch_3 = Concurrent::CountDownLatch.new(1)

        in_thread do
          latch_1.wait(1)
          subject.acquire_write_lock
          latch_2.count_down
          latch_3.wait(1)
          subject.release_write_lock
        end

        in_thread do
          latch_2.wait(1)
          subject.acquire_read_lock
          subject.release_read_lock
        end

        latch_1.count_down
        latch_2.wait(1)

        expect(subject).to have_waiters

        latch_3.count_down
      end

      it 'returns true when the write lock is held and there are waiting writers' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        latch_3 = Concurrent::CountDownLatch.new(1)

        t1 = in_thread do
          latch_1.wait(1)
          subject.acquire_write_lock
          latch_2.count_down
          latch_3.wait(1)
          subject.release_write_lock
        end

        t2 = in_thread do
          latch_2.wait(1)
          subject.acquire_write_lock
          subject.release_write_lock
        end

        latch_1.count_down
        latch_2.wait(1)

        expect(subject).to have_waiters

        latch_3.count_down

        join_with [t1, t2]
      end
    end

    context '#with_read_lock' do

      it 'acquires the lock' do
        expect(subject).to receive(:acquire_read_lock).with(no_args)
        subject.with_read_lock { nil }
      end

      it 'returns the value of the block operation' do
        expected = 100
        actual = subject.with_read_lock { expected }
        expect(actual).to eq expected
      end

      it 'releases the lock' do
        expect(subject).to receive(:release_read_lock).with(no_args)
        subject.with_read_lock { nil }
      end

      it 'raises an exception if no block is given' do
        expect {
          subject.with_read_lock
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception if maximum lock limit is exceeded' do
        counter = Concurrent::AtomicFixnum.new(ReadWriteLock::MAX_READERS)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        expect {
          subject.with_read_lock { nil }
        }.to raise_error(Concurrent::ResourceLimitError)
      end

      it 'releases the lock when an exception is raised' do
        expect(subject).to receive(:release_read_lock).with(any_args)
        begin
          subject.release_read_lock { raise StandardError }
        rescue
        end
      end
    end

    context '#with_write_lock' do

      it 'acquires the lock' do
        expect(subject).to receive(:acquire_write_lock).with(no_args)
        subject.with_write_lock { nil }
      end

      it 'returns the value of the block operation' do
        expected = 100
        actual = subject.with_write_lock { expected }
        expect(actual).to eq expected
      end

      it 'releases the lock' do
        expect(subject).to receive(:release_write_lock).with(no_args)
        subject.with_write_lock { nil }
      end

      it 'raises an exception if no block is given' do
        expect {
          subject.with_write_lock
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception if maximum lock limit is exceeded' do
        counter = Concurrent::AtomicFixnum.new(ReadWriteLock::MAX_WRITERS)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        expect {
          subject.with_write_lock { nil }
        }.to raise_error(Concurrent::ResourceLimitError)
      end

      it 'releases the lock when an exception is raised' do
        expect(subject).to receive(:release_write_lock).with(any_args)
        begin
          subject.with_write_lock { raise StandardError }
        rescue
        end
      end
    end

    context '#acquire_read_lock' do

      it 'increments the lock count' do
        counter = Concurrent::AtomicFixnum.new(0)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        subject.acquire_read_lock
        expect(counter.value).to eq 1
      end

      it 'waits for a running writer to finish' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        latch_3 = Concurrent::CountDownLatch.new(1)

        write_flag = Concurrent::AtomicBoolean.new(false)
        read_flag = Concurrent::AtomicBoolean.new(false)

        thread_1 = in_thread do
          latch_1.wait(1)
          subject.acquire_write_lock
          latch_2.count_down
          latch_3.wait(1)
          write_flag.make_true
          subject.release_write_lock
        end

        thread_2 = in_thread do
          latch_2.wait(1)
          expect(write_flag.value).to be false
          latch_3.count_down
          subject.acquire_read_lock
          expect(write_flag.value).to be true
          read_flag.make_true
          subject.release_read_lock
        end

        latch_1.count_down
        [thread_1, thread_2].each(&:join)

        expect(write_flag.value).to be true
        expect(read_flag.value).to be true
      end

      it 'does not wait for any running readers' do
        counter = Concurrent::AtomicFixnum.new(0)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)

        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        latch_3 = Concurrent::CountDownLatch.new(1)

        read_flag_1 = Concurrent::AtomicBoolean.new(false)
        read_flag_2 = Concurrent::AtomicBoolean.new(false)

        thread_1 = in_thread do
          latch_1.wait(1)
          subject.acquire_read_lock
          expect(counter.value).to eq 1
          latch_2.count_down
          latch_3.wait(1)
          read_flag_1.make_true
          subject.release_read_lock
        end

        thread_2 = in_thread do
          latch_2.wait(1)
          expect(read_flag_1.value).to be false
          subject.acquire_read_lock
          expect(counter.value).to eq 2
          latch_3.count_down
          read_flag_2.make_true
          subject.release_read_lock
        end

        latch_1.count_down
        [thread_1, thread_2].each(&:join)

        expect(read_flag_1.value).to be true
        expect(read_flag_2.value).to be true
        expect(counter.value).to eq 0
      end

      it 'raises an exception if maximum lock limit is exceeded' do
        counter = Concurrent::AtomicFixnum.new(ReadWriteLock::MAX_WRITERS)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        expect {
          subject.acquire_write_lock { nil }
        }.to raise_error(Concurrent::ResourceLimitError)
      end

      it 'returns true if the lock is acquired' do
        expect(subject.acquire_read_lock).to be true
      end
    end

    context '#release_read_lock' do

      it 'decrements the counter' do
        counter = Concurrent::AtomicFixnum.new(0)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        subject.acquire_read_lock
        expect(counter.value).to eq 1
        subject.release_read_lock
        expect(counter.value).to eq 0
      end

      it 'unblocks waiting writers' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        write_flag = Concurrent::AtomicBoolean.new(false)

        thread = in_thread do
          latch_1.wait(1)
          latch_2.count_down
          subject.acquire_write_lock
          write_flag.make_true
          subject.release_write_lock
        end

        subject.acquire_read_lock
        latch_1.count_down
        latch_2.wait(1)
        expect(write_flag.value).to be false
        subject.release_read_lock
        thread.join
        expect(write_flag.value).to be true
      end

      it 'returns true if the lock is released' do
        subject.acquire_read_lock
        expect(subject.release_read_lock).to be true
      end

      it 'returns true if the lock was never set' do
        expect(subject.release_read_lock).to be true
      end
    end

    context '#acquire_write_lock' do

      it 'increments the lock count' do
        counter = Concurrent::AtomicFixnum.new(0)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        subject.acquire_write_lock
        expect(counter.value).to be > 1
      end

      it 'waits for a running writer to finish' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        latch_3 = Concurrent::CountDownLatch.new(1)

        write_flag_1 = Concurrent::AtomicBoolean.new(false)
        write_flag_2 = Concurrent::AtomicBoolean.new(false)

        thread_1 = in_thread do
          latch_1.wait(1)
          subject.acquire_write_lock
          latch_2.count_down
          latch_3.wait(1)
          write_flag_1.make_true
          subject.release_write_lock
        end

        thread_2 = in_thread do
          latch_2.wait(1)
          expect(write_flag_1.value).to be false
          latch_3.count_down
          subject.acquire_write_lock
          expect(write_flag_1.value).to be true
          write_flag_2.make_true
          subject.release_write_lock
        end

        latch_1.count_down
        [thread_1, thread_2].each(&:join)

        expect(write_flag_1.value).to be true
        expect(write_flag_2.value).to be true
      end

      it 'waits for a running reader to finish' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        latch_3 = Concurrent::CountDownLatch.new(1)

        read_flag = Concurrent::AtomicBoolean.new(false)
        write_flag = Concurrent::AtomicBoolean.new(false)

        thread_1 = in_thread do
          latch_1.wait(1)
          subject.acquire_read_lock
          latch_2.count_down
          latch_3.wait(1)
          read_flag.make_true
          subject.release_read_lock
        end

        thread_2 = in_thread do
          latch_2.wait(1)
          expect(read_flag.value).to be false
          latch_3.count_down
          subject.acquire_write_lock
          expect(read_flag.value).to be true
          write_flag.make_true
          subject.release_write_lock
        end

        latch_1.count_down
        [thread_1, thread_2].each(&:join)

        expect(read_flag.value).to be true
        expect(write_flag.value).to be true
      end

      it 'raises an exception if maximum lock limit is exceeded' do
        counter = Concurrent::AtomicFixnum.new(ReadWriteLock::MAX_WRITERS)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        expect {
          subject.acquire_write_lock { nil }
        }.to raise_error(Concurrent::ResourceLimitError)
      end

      it 'returns true if the lock is acquired' do
        expect(subject.acquire_write_lock).to be true
      end
    end

    context '#release_write_lock' do

      it 'decrements the counter' do
        counter = Concurrent::AtomicFixnum.new(0)
        allow(Concurrent::AtomicFixnum).to receive(:new).with(anything).and_return(counter)
        subject.acquire_write_lock
        expect(counter.value).to be > 1
        subject.release_write_lock
        expect(counter.value).to eq 0
      end

      it 'unblocks waiting readers' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        read_flag = Concurrent::AtomicBoolean.new(false)

        thread = in_thread do
          latch_1.wait(1)
          latch_2.count_down
          subject.acquire_read_lock
          read_flag.make_true
          subject.release_read_lock
        end

        subject.acquire_write_lock
        latch_1.count_down
        latch_2.wait(1)
        expect(read_flag.value).to be false
        subject.release_write_lock
        thread.join
        expect(read_flag.value).to be true
      end

      it 'unblocks waiting writers' do
        latch_1 = Concurrent::CountDownLatch.new(1)
        latch_2 = Concurrent::CountDownLatch.new(1)
        write_flag = Concurrent::AtomicBoolean.new(false)

        thread = in_thread do
          latch_1.wait(1)
          latch_2.count_down
          subject.acquire_write_lock
          write_flag.make_true
          subject.release_write_lock
        end

        subject.acquire_write_lock
        latch_1.count_down
        latch_2.wait(1)
        expect(write_flag.value).to be false
        subject.release_write_lock
        thread.join
        expect(write_flag.value).to be true
      end

      it 'returns true if the lock is released' do
        subject.acquire_write_lock
        expect(subject.release_write_lock).to be true
      end

      it 'returns true if the lock was never set' do
        expect(subject.release_write_lock).to be true
      end
    end
  end
end
