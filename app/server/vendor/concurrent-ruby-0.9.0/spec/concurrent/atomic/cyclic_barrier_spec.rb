module Concurrent

  describe CyclicBarrier do

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
          threads = [Thread.new { barrier.wait }, Thread.new { barrier.wait }]
          Thread.pass until threads.all? { |t| t.status == 'sleep' }
          expect(barrier.number_waiting).to eq 2
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
        latch = CountDownLatch.new(1)

        Thread.new { barrier.wait; latch.count_down }
        sleep(0.1)
        barrier.reset
        expect(latch.wait(0.1)).to be_truthy

        expect(barrier).not_to be_broken
        expect(barrier.number_waiting).to eq 0
      end
    end

    describe '#wait' do
      context 'without timeout' do
        it 'should block the thread' do
          t = Thread.new { barrier.wait }
          sleep(0.1)

          expect(t.status).to eq 'sleep'
        end

        it 'should release all threads when their number matches the desired one' do
          latch = CountDownLatch.new(parties)

          parties.times { Thread.new { barrier.wait; latch.count_down } }
          expect(latch.wait(0.1)).to be_truthy
          expect(barrier.number_waiting).to eq 0
          expect(barrier).not_to be_broken
        end

        it 'returns true when released' do
          latch = CountDownLatch.new(parties)

          parties.times { Thread.new { latch.count_down if barrier.wait == true } }
          expect(latch.wait(0.1)).to be_truthy
        end

        it 'executes the block once' do
          counter = AtomicFixnum.new
          barrier = described_class.new(parties) { counter.increment }

          latch = CountDownLatch.new(parties)

          parties.times { Thread.new { latch.count_down if barrier.wait == true } }
          expect(latch.wait(0.1)).to be_truthy

          expect(counter.value).to eq 1
        end

        it 'can be reused' do
          first_latch = CountDownLatch.new(parties)
          parties.times { Thread.new { barrier.wait; first_latch.count_down } }
          expect(first_latch.wait(0.1)).to be_truthy

          latch = CountDownLatch.new(parties)
          parties.times { Thread.new { barrier.wait; latch.count_down } }
          expect(latch.wait(0.1)).to be_truthy
        end

        it 'return false if barrier has been reset' do
          latch = CountDownLatch.new(1)

          Thread.new { latch.count_down if barrier.wait == false }
          sleep(0.1)
          barrier.reset
          expect(latch.wait(0.1)).to be_truthy
        end
      end

      context 'with timeout' do
        context 'timeout not expiring' do
          it 'should block the thread' do
            t = Thread.new { barrier.wait(1) }
            sleep(0.1)

            expect(t.status).to eq 'sleep'
          end

          it 'should release all threads when their number matches the desired one' do
            latch = CountDownLatch.new(parties)

            parties.times { Thread.new { barrier.wait(1); latch.count_down } }
            expect(latch.wait(0.2)).to be_truthy
            expect(barrier.number_waiting).to eq 0
          end

          it 'returns true when released' do
            latch = CountDownLatch.new(parties)

            parties.times { Thread.new { latch.count_down if barrier.wait(1) == true } }
            expect(latch.wait(0.1)).to be_truthy
          end
        end

        context 'timeout expiring' do

          it 'returns false' do
            latch = CountDownLatch.new(1)

            Thread.new { latch.count_down if barrier.wait(0.1) == false }
            expect(latch.wait(0.2)).to be_truthy
          end

          it 'breaks the barrier and release all other threads' do
            latch = CountDownLatch.new(2)

            Thread.new { barrier.wait(0.1); latch.count_down }
            Thread.new { barrier.wait; latch.count_down }

            expect(latch.wait(0.2)).to be_truthy
            expect(barrier).to be_broken
          end

          it 'breaks the barrier and release all other threads 2' do
            t1 = Thread.new { barrier.wait(0.1) }
            t2 = Thread.new { barrier.wait(0.1) }

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
          Thread.new { barrier.wait(0.1) }
          sleep(0.2)

          expect(barrier).to be_broken

          expect(barrier.wait).to be_falsey
        end

        it 'can be reset' do
          Thread.new { barrier.wait(0.1) }
          sleep(0.2)

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
        Thread.new { barrier.wait; @expected = true }

        sleep(0.1)
        barrier.simulate_spurious_wake_up

        sleep(0.1)
        expect(@expected).to be_falsey
      end

      it 'should resist to spurious wake ups with timeout' do
        @expected = false
        Thread.new { barrier.wait(0.5); @expected = true }

        sleep(0.1)
        barrier.simulate_spurious_wake_up

        sleep(0.1)
        expect(@expected).to be_falsey

      end
    end
  end
end
