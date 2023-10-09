require 'concurrent/atomic/thread_local_var'
require 'concurrent/atomic/count_down_latch'

module Concurrent

  RSpec.describe ThreadLocalVar do

    context '#initialize' do

      it 'can set an initial value' do
        v = described_class.new(14)
        expect(v.value).to eq 14
      end

      it 'sets nil as a default initial value' do
        v = described_class.new
        expect(v.value).to be_nil
      end

      it 'sets the same initial value for all threads' do
        v  = described_class.new(14)
        t1 = in_thread { v.value }
        t2 = in_thread { v.value }
        expect(t1.value).to eq 14
        expect(t2.value).to eq 14
      end

      it 'can set a block to be called to get the initial value' do
        v = described_class.new { 14 }
        expect(v.value).to eq 14
      end

      context 'when attempting to set both an initial value and a block' do
        it do
          expect { described_class.new(14) { 14 } }.to raise_error(ArgumentError)
        end
      end
    end

    context '#value' do
      let(:v) { described_class.new(14) }

      it 'returns the current value' do
        expect(v.value).to eq 14
      end

      it 'returns the value after modification' do
        v.value = 2
        expect(v.value).to eq 2
      end

      context 'when using a block to initialize the value' do
        it 'calls the block to initialize the value' do
          block = proc { }

          expect(block).to receive(:call)

          v = described_class.new(&block)
          v.value
        end

        it 'sets the block return value as the current value' do
          value = 13

          v = described_class.new { value += 1 }

          v.value
          expect(v.value).to be 14
        end

        it 'calls the block to initialize the value for each thread' do
          block = proc { }

          expect(block).to receive(:call).twice

          v = described_class.new(&block)
          in_thread { v.value }.join
          in_thread { v.value }.join
        end
      end
    end

    context '#value=' do
      let(:v) { described_class.new(14) }

      it 'sets a new value' do
        v.value = 2
        expect(v.value).to eq 2
      end

      it 'returns the new value' do
        expect(v.value = 2).to eq 2
      end

      it 'does not modify the initial value for other threads' do
        v.value = 2
        t = in_thread { v.value }
        expect(t.value).to eq 14
      end

      it 'does not modify the value for other threads' do
        v.value = 3

        b1 = CountDownLatch.new(2)
        b2 = CountDownLatch.new(2)

        t1 = in_thread do
          b1.count_down
          b1.wait
          v.value = 1
          b2.count_down
          b2.wait
          v.value
        end

        t2 = in_thread do
          b1.count_down
          b1.wait
          v.value = 2
          b2.count_down
          b2.wait
          v.value
        end

        expect(t1.value).to eq 1
        expect(t2.value).to eq 2
      end
    end
  end
end
