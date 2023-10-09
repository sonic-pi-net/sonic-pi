require 'concurrent/atomic/fiber_local_var'

module Concurrent

  RSpec.describe FiberLocalVar do

    context '#initialize' do

      it 'can set an initial value' do
        v = described_class.new(14)
        expect(v.value).to eq 14
      end

      it 'sets nil as a default initial value' do
        v = described_class.new
        expect(v.value).to be_nil
      end

      it 'sets the same initial value for all fibers' do
        v = described_class.new(14)
        f1 = in_fiber { v.value }
        f2 = in_fiber { v.value }
        expect(f1.resume).to eq 14
        expect(f2.resume).to eq 14
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

        it 'calls the block to initialize the value for each fiber' do
          block = proc { }

          expect(block).to receive(:call).twice

          v = described_class.new(&block)
          in_fiber { v.value }.resume
          in_fiber { v.value }.resume
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

      it 'does not modify the initial value for other fibers' do
        v.value = 2
        f = in_fiber { v.value }
        expect(f.resume).to eq 14
      end

      it 'does not modify the value for other fibers' do
        v.value = 3

        f1 = in_fiber do
          v.value = 1
          Fiber.yield
          v.value
        end

        f2 = in_fiber do
          v.value = 2
          Fiber.yield
          v.value
        end

        f1.resume
        f2.resume

        expect(f1.resume).to eq 1
        expect(f2.resume).to eq 2
      end
    end
  end
end
