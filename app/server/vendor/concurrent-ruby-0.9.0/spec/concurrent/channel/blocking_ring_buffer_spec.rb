module Concurrent
  module Channel

    describe BlockingRingBuffer do

      let(:capacity) { 3 }
      let(:buffer) { BlockingRingBuffer.new(capacity) }

      def fill_buffer
        capacity.times { buffer.put 3 }
      end

      describe '#capacity' do
        it 'returns the value passed in constructor' do
          expect(buffer.capacity).to eq capacity
        end
      end

      describe '#count' do
        it 'is zero when created' do
          expect(buffer.count).to eq 0
        end

        it 'increases when an element is added' do
          buffer.put 5
          expect(buffer.count).to eq 1

          buffer.put 1
          expect(buffer.count).to eq 2
        end

        it 'decreases when an element is removed' do
          buffer.put 10

          buffer.take

          expect(buffer.count).to eq 0
        end
      end

      describe '#empty?' do
        it 'is true when count is zero' do
          expect(buffer.empty?).to be_truthy
        end

        it 'is false when count is not zero' do
          buffer.put 82
          expect(buffer.empty?).to be_falsey
        end
      end

      describe '#full?' do
        it 'is true when count is capacity' do
          fill_buffer
          expect(buffer.full?).to be_truthy
        end

        it 'is false when count is not capacity' do
          expect(buffer.full?).to be_falsey
        end
      end

      describe '#put' do
        it 'block when buffer is full' do
          fill_buffer

          t = Thread.new { buffer.put 32 }

          sleep(0.1)

          expect(t.status).to eq 'sleep'
        end

        it 'continues when an element is removed' do
          latch = CountDownLatch.new(1)

          Thread.new { (capacity + 1).times { buffer.put 'hi' }; latch.count_down }
          Thread.new { sleep(0.1); buffer.take }

          expect(latch.wait(0.2)).to be_truthy
        end
      end

      describe '#take' do
        it 'blocks when buffer is empty' do
          t = Thread.new { buffer.take }

          sleep(0.1)

          expect(t.status).to eq 'sleep'
        end

        it 'continues when an element is added' do
          latch = CountDownLatch.new(1)

          Thread.new { buffer.take; latch.count_down }
          Thread.new { sleep(0.1); buffer.put 3 }

          expect(latch.wait(0.2)).to be_truthy
        end

        it 'returns the first added value' do
          buffer.put 'hi'
          buffer.put 'foo'
          buffer.put 'bar'

          expect(buffer.take).to eq 'hi'
          expect(buffer.take).to eq 'foo'
          expect(buffer.take).to eq 'bar'
        end
      end

      describe '#peek' do
        context 'buffer empty' do
          it 'returns nil when buffer is empty' do
            expect(buffer.peek).to be_nil
          end
        end

        context 'not empty' do

          before(:each) { buffer.put 'element' }

          it 'returns the first value' do
            expect(buffer.peek).to eq 'element'
          end

          it 'does not change buffer' do
            buffer.peek
            expect(buffer.count).to eq 1
          end
        end
      end

      context 'circular condition' do
        it 'can filled many times' do
          fill_buffer
          capacity.times { buffer.take }

          buffer.put 'hi'

          expect(buffer.take).to eq 'hi'
          expect(buffer.capacity).to eq capacity
        end
      end

    end
  end
end
