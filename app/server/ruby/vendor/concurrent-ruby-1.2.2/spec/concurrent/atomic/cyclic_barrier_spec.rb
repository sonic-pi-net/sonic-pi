require 'concurrent/atomic/cyclic_barrier'
require 'concurrent/atomic/count_down_latch'
require 'concurrent/atomic/atomic_fixnum'

module Concurrent

  RSpec.describe CyclicBarrier do

    let(:parties) { 3 }
    let!(:barrier) { described_class.new(3) }

    context '#initialize' do

      it 'raises an exception if the initial count is less than 1' do
        expect {
          described_class.new(0)
        }.to raise_error(ArgumentError)
      end

      it 'raises an exception if the initial count is not an integer' do
        expect {
          described_class.new('foo')
        }.to raise_error(ArgumentError)
      end
    end

    describe '#parties' do

      it 'should be the value passed to the constructor' do
        expect(barrier.parties).to eq 3
      end

    end

    describe '#number_waiting' do
      context 'without any waiting thread' do
        it 'should be equal to zero' do
          expect(barrier.number_waiting).to eq 0
        end
      end

      context 'with waiting threads' do
        it 'should be equal to the waiting threads count' do
          in_thread { barrier.wait }
          in_thread { barrier.wait }
          repeat_until_success { expect(barrier.number_waiting).to eq 2 }
        end
      end
    end

    describe '#broken?' do
      it 'should not be broken when created' do
        expect(barrier.broken?).to eq false
      end

      it 'should not be broken when reset is called without waiting thread' do
        barrier.reset
        expect(barrier.broken?).to eq false
      end
    end

    describe 'reset' do
      it 'should release all waiting threads' do
        start_latch    = CountDownLatch.new(1)
        continue_latch = CountDownLatch.new(1)

        in_thread do
          start_latch.count_down
          barrier.wait
          continue_latch.count_down
        end

        start_latch.wait(1)
        barrier.reset

        expect(barrier).not_to be_broken
        expect(barrier.number_waiting).to eq 0
      end
    end

    describe '#wait' do
      context 'without timeout' do
        it 'should block the thread' do
          t = in_thread { barrier.wait }
          t.join(0.1)

          expect(t.status).to eq 'sleep'
        end

        it 'should release all threads when their number matches the desired one' do
          latch = CountDownLatch.new(parties)

          parties.times { in_thread { barrier.wait; latch.count_down } }
          expect(latch.wait(1)).to be_truthy
          expect(barrier.number_waiting).to eq 0
          expect(barrier).not_to be_broken
        end

        it 'returns true when released' do
          latch = CountDownLatch.new(parties)

          parties.times { in_thread { latch.count_down if barrier.wait == true } }
          expect(latch.wait(1)).to be_truthy
        end

        it 'executes the block once' do
          counter = AtomicFixnum.new
          barrier = described_class.new(parties) { counter.increment }

          latch = CountDownLatch.new(parties)

          parties.times { in_thread { latch.count_down if barrier.wait == true } }
          expect(latch.wait(1)).to be_truthy

          expect(counter.value).to eq 1
        end

        it 'can be reused' do
          first_latch = CountDownLatch.new(parties)
          parties.times { in_thread { barrier.wait; first_latch.count_down } }

          latch = CountDownLatch.new(parties)
          parties.times { in_thread { barrier.wait; latch.count_down } }
          expect(latch.wait(1)).to be_truthy
        end

        it 'return false if barrier has been reset' do
          latch = CountDownLatch.new(1)

          t = in_thread { latch.count_down if barrier.wait == false }
          t.join(0.1)
          barrier.reset
          expect(latch.wait(1)).to be_truthy
        end
      end

      context 'with timeout' do
        context 'timeout not expiring' do
          it 'should block the thread' do
            t = in_thread { barrier.wait(1) }
            t.join(0.1)

            expect(t.status).to eq 'sleep'
          end

          it 'should release all threads when their number matches the desired one' do
            latch = CountDownLatch.new(parties)

            parties.times { in_thread { barrier.wait(1); latch.count_down } }
            expect(latch.wait(0.2)).to be_truthy
            expect(barrier.number_waiting).to eq 0
          end

          it 'returns true when released' do
            latch = CountDownLatch.new(parties)

            parties.times { in_thread { latch.count_down if barrier.wait(1) == true } }
            expect(latch.wait(1)).to be_truthy
          end
        end

        context 'timeout expiring' do

          it 'returns false' do
            latch = CountDownLatch.new(1)

            in_thread { latch.count_down if barrier.wait(0.1) == false }
            expect(latch.wait(1)).to be_truthy
          end

          it 'breaks the barrier and release all other threads' do
            latch = CountDownLatch.new(2)

            in_thread { barrier.wait(0.1); latch.count_down }
            in_thread { barrier.wait; latch.count_down }

            expect(latch.wait(1)).to be_truthy
            expect(barrier).to be_broken
          end

          it 'breaks the barrier and release all other threads 2' do
            t1 = in_thread { barrier.wait(0.1) }
            t2 = in_thread { barrier.wait(0.1) }

            [t1, t2].each(&:join)

            expect(barrier).to be_broken
          end

          it 'does not execute the block on timeout' do
            counter = AtomicFixnum.new
            barrier = described_class.new(parties) { counter.increment }

            barrier.wait(0.1)

            expect(counter.value).to eq 0
          end
        end
      end

      context '#broken barrier' do
        it 'should not accept new threads' do
          t = in_thread { barrier.wait(0.01) }
          join_with t

          expect(barrier).to be_broken
          expect(barrier.wait).to be_falsey
        end

        it 'can be reset' do
          t = in_thread { barrier.wait(0.01) }
          join_with t

          expect(barrier).to be_broken

          barrier.reset

          expect(barrier).not_to be_broken
        end
      end
    end

    context 'spurious wake ups' do

      before(:each) do
        def barrier.simulate_spurious_wake_up
          synchronize do
            ns_signal
            ns_broadcast
          end
        end
      end

      it 'should resist to spurious wake ups without timeout' do
        @expected = false
        t         = in_thread { barrier.wait; @expected = true }
        t.join(0.1)

        barrier.simulate_spurious_wake_up

        t.join(0.1)
        expect(@expected).to be_falsey
      end

      it 'should resist to spurious wake ups with timeout' do
        @expected = false
        t         = in_thread { barrier.wait(0.5); @expected = true }

        t.join(0.1)
        barrier.simulate_spurious_wake_up

        t.join(0.1)
        expect(@expected).to be_falsey

      end
    end
  end
end
