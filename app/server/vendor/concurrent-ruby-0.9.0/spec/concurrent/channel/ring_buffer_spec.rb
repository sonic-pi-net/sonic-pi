module Concurrent
  module Channel

    describe RingBuffer do

      let(:capacity) { 3 }
      let(:buffer) { RingBuffer.new(capacity) }

      def fill_buffer
        capacity.times { buffer.offer 3 }
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
          buffer.offer 5
          expect(buffer.count).to eq 1

          buffer.offer 1
          expect(buffer.count).to eq 2
        end

        it 'decreases when an element is removed' do
          buffer.offer 10
          buffer.poll

          expect(buffer.count).to eq 0
        end
      end

      describe '#empty?' do
        it 'is true when count is zero' do
          expect(buffer.empty?).to be_truthy
        end

        it 'is false when count is not zero' do
          buffer.offer 82
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

      describe '#offer' do
        it 'returns false when buffer is full' do
          fill_buffer
          expect(buffer.offer(3)).to be_falsey
        end

        it 'returns true when the buffer is not full' do
          expect(buffer.offer(5)).to be_truthy
        end

      end

      describe '#poll' do
        it 'returns the first added value' do
          buffer.offer 'hi'
          buffer.offer 'foo'
          buffer.offer 'bar'

          expect(buffer.poll).to eq 'hi'
          expect(buffer.poll).to eq 'foo'
          expect(buffer.poll).to eq 'bar'
        end

        it 'returns nil when buffer is empty' do
          expect(buffer.poll).to be_nil
        end
      end

      describe '#peek' do
        context 'buffer empty' do
          it 'returns nil when buffer is empty' do
            expect(buffer.peek).to be_nil
          end
        end

        context 'not empty' do

          before(:each) { buffer.offer 'element' }

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
          capacity.times { buffer.poll }

          buffer.offer 'hi'

          expect(buffer.poll).to eq 'hi'
          expect(buffer.capacity).to eq capacity
        end
      end

    end
  end
end
