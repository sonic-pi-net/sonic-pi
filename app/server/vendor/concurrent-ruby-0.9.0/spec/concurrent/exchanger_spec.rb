module Concurrent

  describe Exchanger, notravis: true do

    describe 'exchange' do

      context 'without timeout' do

        it 'should block' do
          latch_1 = Concurrent::CountDownLatch.new
          latch_2 = Concurrent::CountDownLatch.new

          t = Thread.new do
            latch_1.count_down
            subject.exchange(1)
            latch_2.count_down
          end

          latch_1.wait(1)
          latch_2.wait(0.1)
          expect(latch_2.count).to eq 1
          t.kill
        end

        it 'should receive the other value' do
          first_value = nil
          second_value = nil
          latch = Concurrent::CountDownLatch.new(2)

          threads = [
            Thread.new { first_value = subject.exchange(2); latch.count_down },
            Thread.new { second_value = subject.exchange(4); latch.count_down }
          ]

          latch.wait(1)

          expect(first_value).to eq 4
          expect(second_value).to eq 2

          threads.each {|t| t.kill }
        end

        it 'can be reused' do
          first_value = nil
          second_value = nil
          latch_1 = Concurrent::CountDownLatch.new(2)
          latch_2 = Concurrent::CountDownLatch.new(2)

          threads = [
            Thread.new { first_value = subject.exchange(1); latch_1.count_down },
            Thread.new { second_value = subject.exchange(0); latch_1.count_down }
          ]

          latch_1.wait(1)
          threads.each {|t| t.kill }

          threads = [
            Thread.new { first_value = subject.exchange(10); latch_2.count_down },
            Thread.new { second_value = subject.exchange(12); latch_2.count_down }
          ]

          latch_2.wait(1)
          expect(first_value).to eq 12
          expect(second_value).to eq 10
          threads.each {|t| t.kill }
        end
      end

      context 'with timeout' do

        it 'should block until timeout' do
          duration = Concurrent::TestHelpers.monotonic_interval do
            subject.exchange(2, 0.1)
          end
          expect(duration).to be_within(0.05).of(0.1)
        end
      end
    end
  end
end
