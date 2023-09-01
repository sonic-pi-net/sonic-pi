require 'concurrent/utility/engine'
require 'concurrent/atomic/reentrant_read_write_lock'
require 'concurrent/atomic/count_down_latch'
require 'concurrent/atomic/atomic_boolean'
require 'concurrent/set'
require 'timeout'
require 'fiber'

module Concurrent
  BaseMatcher = RSpec::Matchers::BuiltIn::BaseMatcher

  class TrackedReentrantReadWriteLock < ReentrantReadWriteLock
    def initialize(scope = Thread)
      super()

      @scope = scope
      @read_acquired = Concurrent::Set.new
      @write_acquired = Concurrent::Set.new
    end

    attr :read_acquired
    attr :write_acquired

    def try_read_lock
      if super
        @read_acquired.add(@scope.current)
        true
      else
        false
      end
    end

    def try_write_lock
      if super
        @write_acquired.add(@scope.current)
        true
      else
        false
      end
    end

    def acquire_read_lock
      if super
        @read_acquired.add(@scope.current)
        true
      else
        false
      end
    end

    def acquire_write_lock
      if super
        @write_acquired.add(@scope.current)
        true
      else
        false
      end
    end

    def release_read_lock
      super.tap do
        @read_acquired.delete(@scope.current)
      end
    end

    def release_write_lock
      super.tap do
        @write_acquired.delete(@scope.current)
      end
    end
  end

  # ****************************************************************
  # First some custom matchers to make our tests all nice and pretty
  # ****************************************************************

  class HoldLock
    def initialize(lock)
      @lock = lock
    end

    def for_read
      HoldReadLock.new(@lock)
    end

    def for_write
      HoldWriteLock.new(@lock)
    end

    def for_both
      HoldBoth.new(@lock)
    end
  end

  class HoldReadLock < BaseMatcher
    def match(lock, scope)
      ((lock.instance_eval { @Counter.value } & ReentrantReadWriteLock::MAX_READERS) != 0) && lock.read_acquired.include?(scope)
    end
  end

  class HoldWriteLock < BaseMatcher
    def match(lock, scope)
      ((lock.instance_eval { @Counter.value } & ReentrantReadWriteLock::RUNNING_WRITER) != 0) && lock.write_acquired.include?(scope)
    end
  end

  class HoldBoth < BaseMatcher
    def match(lock, scope)
      HoldReadLock.new(lock).matches?(scope) && HoldWriteLock.new(lock).matches?(scope)
    end
  end

  class BeFree < BaseMatcher
    MASK = ReentrantReadWriteLock::MAX_READERS + ReentrantReadWriteLock::RUNNING_WRITER

    def matches?(lock)
      (lock.instance_eval { @Counter.value } & MASK) == 0
    end

    def failure_message
      "expected lock to be free"
    end
  end

  # *******************************************************

  RSpec.shared_context TrackedReentrantReadWriteLock do
    def hold(lock)
      HoldLock.new(lock)
    end

    def be_free
      BeFree.new
    end

    def wait_up_to(secs, &condition)
      _end = Time.now + secs
      while !condition.call && Time.now < _end
        sleep(0.001)
      end
    end
  end

  RSpec.describe ReentrantReadWriteLock do
    include_context TrackedReentrantReadWriteLock

    let(:lock) { TrackedReentrantReadWriteLock.new }

    context "read lock" do

      it "allows other read locks to be acquired at the same time" do
        lock # stupid RSpec 'let' is not thread-safe!
        Timeout.timeout(3) do
          got_lock = 10.times.collect { CountDownLatch.new }
          threads  = 10.times.collect do |n|
            in_thread do
              # Each thread takes the read lock and then waits for another one
              # They will only finish if ALL of them get their read lock
              expect(lock.acquire_read_lock).to be true
              expect(Thread.current).to hold(lock).for_read
              got_lock[n].count_down
              got_lock[(n+1) % 10].wait
            end
          end
          threads.each(&:join)
        end
      end

      it "can be acquired more than once" do
        Timeout.timeout(3) do
          10.times { expect(lock.acquire_read_lock).to be true }
          expect(Thread.current).to hold(lock).for_read
          10.times { expect(lock.release_read_lock).to be true }
          expect(Thread.current).not_to hold(lock).for_read
          expect(lock).to be_free
        end
      end

      it "can be acquired while holding a write lock" do
        Timeout.timeout(3) do
          expect(lock.acquire_write_lock).to be true
          expect(Thread.current).to hold(lock).for_write
          expect(lock.acquire_read_lock).to be true
          expect(Thread.current).to hold(lock).for_both
          expect(lock.release_read_lock).to be true
          expect(Thread.current).to hold(lock).for_write
          expect(Thread.current).not_to hold(lock).for_read
          expect(lock.release_write_lock).to be true
          expect(lock).to be_free
        end
      end

      it "can be upgraded to a write lock" do
        Timeout.timeout(3) do
          expect(lock.acquire_read_lock).to be true
          expect(Thread.current).to hold(lock).for_read
          # now we want to upgrade...
          expect(lock.acquire_write_lock).to be true
          expect(lock.release_read_lock).to be true
          expect(Thread.current).to hold(lock).for_write

          expect(lock.release_write_lock).to be true
          expect(lock).to be_free
        end
      end

      it "can be upgraded to a write lock when read lock acquired more than once" do
        Timeout.timeout(3) do
          expect(lock.acquire_read_lock).to be true
          expect(lock.acquire_read_lock).to be true
          expect(Thread.current).to hold(lock).for_read

          # now we want to upgrade...
          expect(lock.acquire_write_lock).to be true
          expect(lock.release_read_lock).to be true
          expect(lock.release_read_lock).to be true
          expect(Thread.current).to hold(lock).for_write

          expect(lock.release_write_lock).to be true
          expect(lock).to be_free
        end
      end

      it "cannot be released when not held" do
        expect { lock.release_read_lock }.to raise_error(IllegalOperationError)
      end

      it "cannot be released more times than it was taken" do
        Timeout.timeout(3) do
          2.times { lock.acquire_read_lock }
          2.times { lock.release_read_lock }
          expect { lock.release_read_lock }.to raise_error(IllegalOperationError)
        end
      end

      it "wakes up waiting writers when the last read lock is released" do
        latch1,latch2 = CountDownLatch.new(3),CountDownLatch.new
        good = AtomicBoolean.new(false)
        threads = [
          in_thread { lock.acquire_read_lock; latch1.count_down; latch2.wait; lock.release_read_lock },
          in_thread { lock.acquire_read_lock; latch1.count_down; latch2.wait; lock.release_read_lock },
          in_thread { lock.acquire_read_lock; latch1.count_down; latch2.wait; lock.release_read_lock },
          in_thread { latch1.wait; lock.acquire_write_lock; good.value = true }
        ]
        wait_up_to(0.2) { threads[3].status == 'sleep' }
        # The last thread should be waiting to acquire a write lock now...
        expect(threads[3].status).to eql "sleep"
        expect(threads[3]).not_to hold(lock).for_write
        expect(good.value).to be false
        # Throw latch2 and the 3 readers will wake up and all release their read locks...
        latch2.count_down
        wait_up_to(0.2) { good.value }
        expect(threads[3]).to hold(lock).for_write
        expect(good.value).to be true
      end
    end

    context "write lock" do
      it "cannot be acquired when another thread holds a write lock" do
        latch   = CountDownLatch.new
        threads = [
          in_thread { lock.acquire_write_lock; latch.count_down },
          in_thread { latch.wait; lock.acquire_write_lock }
        ]
        expect { Timeout.timeout(1) { threads[0].join }}.not_to raise_error
        expect(threads[0]).to hold(lock).for_write
        expect(threads[1]).not_to hold(lock).for_write
        wait_up_to(0.2) { threads[1].status == 'sleep' }
        expect(threads[1].status).to eql "sleep"
      end

      it "cannot be acquired when another thread holds a read lock" do
        latch   = CountDownLatch.new
        threads = [
          in_thread { lock.acquire_read_lock; latch.count_down },
          in_thread { latch.wait; lock.acquire_write_lock }
        ]
        expect { Timeout.timeout(1) { threads[0].join }}.not_to raise_error
        expect(threads[0]).to hold(lock).for_read
        expect(threads[1]).not_to hold(lock).for_write
        wait_up_to(0.2) { threads[1].status == 'sleep' }
        expect(threads[1].status).to eql "sleep"
      end

      it "can be acquired more than once" do
        Timeout.timeout(3) do
          10.times { expect(lock.acquire_write_lock).to be true }
          expect(Thread.current).to hold(lock).for_write
          10.times { expect(lock.release_write_lock).to be true }
          expect(Thread.current).not_to hold(lock).for_write
          expect(lock).to be_free
        end
      end

      it "can be acquired while holding a read lock" do
        Timeout.timeout(3) do
          expect(lock.acquire_read_lock).to be true
          expect(Thread.current).to hold(lock).for_read
          expect(lock.acquire_write_lock).to be true
          expect(Thread.current).to hold(lock).for_both
          expect(lock.release_write_lock).to be true
          expect(Thread.current).to hold(lock).for_read
          expect(Thread.current).not_to hold(lock).for_write
          expect(lock.release_read_lock).to be true
          expect(lock).to be_free
        end
      end

      it "can be downgraded to a read lock" do
        Timeout.timeout(3) do
          expect(lock.acquire_write_lock).to be true
          expect(Thread.current).to hold(lock).for_write
          # now we want to downgrade...
          expect(lock.acquire_read_lock).to be true
          expect(lock.release_write_lock).to be true
          expect(Thread.current).to hold(lock).for_read

          expect(lock.release_read_lock).to be true
          expect(lock).to be_free
        end
      end

      it "cannot be released when not held" do
        expect { lock.release_write_lock }.to raise_error(IllegalOperationError)
      end

      it "cannot be released more times than it was taken" do
        Timeout.timeout(3) do
          2.times { lock.acquire_write_lock }
          2.times { lock.release_write_lock }
          expect { lock.release_write_lock }.to raise_error(IllegalOperationError)
        end
      end

      it "wakes up waiting readers when the write lock is released" do
        latch1,latch2 = CountDownLatch.new,CountDownLatch.new
        good = AtomicFixnum.new(0)
        threads = [
          in_thread { lock.acquire_write_lock; latch1.count_down; latch2.wait; lock.release_write_lock },
          in_thread { latch1.wait; lock.acquire_read_lock; good.update { |n| n+1 }},
          in_thread { latch1.wait; lock.acquire_read_lock; good.update { |n| n+1 }},
          in_thread { latch1.wait; lock.acquire_read_lock; good.update { |n| n+1 }}
        ]
        wait_up_to(0.2) { threads[3].status == 'sleep' }
        # The last 3 threads should be waiting to acquire read locks now...
        unless Concurrent.on_jruby? # flaky on JRuby
          (1..3).each { |n| expect(threads[n].status).to eql "sleep" }
        end
        (1..3).each { |n| expect(threads[n]).not_to hold(lock).for_read }
        # Throw latch2 and the writer will wake up and release its write lock...
        latch2.count_down
        wait_up_to(0.2) { good.value == 3 }
        (1..3).each { |n| expect(threads[n]).to hold(lock).for_read }
      end

      it "wakes up waiting writers when the write lock is released" do
        latch1,latch2 = CountDownLatch.new,CountDownLatch.new
        good = AtomicBoolean.new(false)
        threads = [
          in_thread { lock.acquire_write_lock; latch1.count_down; latch2.wait; lock.release_write_lock },
          in_thread { latch1.wait; lock.acquire_write_lock; good.value = true },
        ]
        wait_up_to(0.2) { threads[1].status == 'sleep' }
        # The last thread should be waiting to acquire a write lock now...
        unless Concurrent.on_jruby? # flaky on JRuby
          expect(threads[1].status).to eql "sleep"
        end
        expect(threads[1]).not_to hold(lock).for_write
        # Throw latch2 and the writer will wake up and release its write lock...
        latch2.count_down
        wait_up_to(0.2) { good.value }
        expect(threads[1]).to hold(lock).for_write
      end
    end

    context "#with_read_lock" do

      it "acquires read block before yielding, then releases it" do
        expect(lock).to be_free
        lock.with_read_lock { expect(Thread.current).to hold(lock).for_read }
        expect(lock).to be_free
      end

      it "releases read lock if an exception is raised in block" do
        expect {
          lock.with_read_lock { raise "Bad" }
        }.to raise_error(RuntimeError, 'Bad')
        expect(lock).to be_free
        expect(Thread.current).not_to hold(lock).for_read
      end
    end

    context "#with_write_lock" do

      it "acquires write block before yielding, then releases it" do
        expect(lock).to be_free
        lock.with_write_lock { expect(Thread.current).to hold(lock).for_write }
        expect(lock).to be_free
      end

      it "releases write lock if an exception is raised in block" do
        expect {
          lock.with_write_lock { raise "Bad" }
        }.to raise_error(RuntimeError, 'Bad')
        expect(lock).to be_free
        expect(Thread.current).not_to hold(lock).for_write
      end
    end

    context "#try_read_lock" do

      it "returns false immediately if read lock cannot be obtained" do
        Timeout.timeout(3) do
          latch  = CountDownLatch.new
          in_thread { lock.acquire_write_lock; latch.count_down }

          latch.wait
          expect {
            Timeout.timeout(0.01) { expect(lock.try_read_lock).to be false }
          }.not_to raise_error
          expect(Thread.current).not_to hold(lock).for_read
        end
      end

      it "acquires read lock and returns true if it can do so without blocking" do
        Timeout.timeout(3) do
          latch  = CountDownLatch.new
          in_thread { lock.acquire_read_lock; latch.count_down }

          latch.wait
          expect {
            Timeout.timeout(0.01) { expect(lock.try_read_lock).to be true }
          }.not_to raise_error
          expect(lock).not_to be_free
          expect(Thread.current).to hold(lock).for_read
        end
      end

      it "can acquire a read lock if a read lock is already held" do
        Timeout.timeout(3) do
          expect(lock.acquire_read_lock).to be true
          expect(lock.try_read_lock).to be true
          expect(Thread.current).to hold(lock).for_read
          expect(lock.release_read_lock).to be true
          expect(lock.release_read_lock).to be true
          expect(Thread.current).not_to hold(lock).for_read
          expect(lock).to be_free
        end
      end

      it "can acquire a read lock if a write lock is already held" do
        Timeout.timeout(3) do
          expect(lock.acquire_write_lock).to be true
          expect(lock.try_read_lock).to be true
          expect(Thread.current).to hold(lock).for_read
          expect(lock.release_read_lock).to be true
          expect(lock.release_write_lock).to be true
          expect(Thread.current).not_to hold(lock).for_read
          expect(lock).to be_free
        end
      end
    end

    context "#try_write_lock" do

      it "returns false immediately if write lock cannot be obtained" do
        Timeout.timeout(3) do
          latch  = CountDownLatch.new
          in_thread { lock.acquire_write_lock; latch.count_down }

          latch.wait
          expect {
            Timeout.timeout(0.02) { expect(lock.try_write_lock).to be false }
          }.not_to raise_error
          expect(Thread.current).not_to hold(lock).for_write
        end
      end

      it "acquires write lock and returns true if it can do so without blocking" do
        Timeout.timeout(3) do
          expect {
            Timeout.timeout(0.02) { expect(lock.try_write_lock).to be true }
          }.not_to raise_error
          expect(lock).not_to be_free
          expect(Thread.current).to hold(lock).for_write
        end
      end

      it "can acquire a write lock if a read lock is already held" do
        Timeout.timeout(3) do
          expect(lock.acquire_read_lock).to be true
          expect(lock.try_write_lock).to be true
          expect(Thread.current).to hold(lock).for_write
          expect(lock.release_write_lock).to be true
          expect(lock.release_read_lock).to be true
          expect(Thread.current).not_to hold(lock).for_write
          expect(lock).to be_free
        end
      end

      it "can acquire a write lock if a write lock is already held" do
        Timeout.timeout(3) do
          expect(lock.acquire_write_lock).to be true
          expect(lock.try_write_lock).to be true
          expect(Thread.current).to hold(lock).for_write
          expect(lock.release_write_lock).to be true
          expect(lock.release_write_lock).to be true
          expect(Thread.current).not_to hold(lock).for_write
          expect(lock).to be_free
        end
      end
    end

    it "can survive a torture test" do
      count = 0
      writers = 5.times.collect do
        in_thread do
          500.times do
            lock.with_write_lock do
              value = (count += 1)
              sleep(0.0001)
              count = value+1
            end
          end
        end
      end
      readers = 15.times.collect do
        in_thread do
          500.times do
            lock.with_read_lock { expect(count % 2).to eq 0 }
          end
        end
      end
      writers.each(&:join)
      readers.each(&:join)
      expect(count).to eq 5000
    end
  end

  RSpec.describe Concurrent::ReentrantReadWriteLock, if: (Concurrent::LockLocalVar == Concurrent::FiberLocalVar) do
    include_context TrackedReentrantReadWriteLock

    let(:lock) { Concurrent::TrackedReentrantReadWriteLock.new(Fiber) }

    it "can acquire locks in separate fibers" do
      lock.with_read_lock do
        expect(Fiber.current).to hold(lock).for_read
        Fiber.new do
          expect(Fiber.current).to_not hold(lock).for_read

          lock.with_read_lock do
            expect(Fiber.current).to hold(lock).for_read
          end

        end.resume

        expect(Fiber.current).to hold(lock).for_read
      end
    end
  end
end
