require 'concurrent/array'

module Concurrent
  RSpec.describe Array do
    let!(:ary) { described_class.new }

    describe '.[]' do
      describe 'when initializing with no arguments' do
        it do
          expect(described_class[]).to be_empty
        end
      end

      describe 'when initializing with arguments' do
        it 'creates an array with the given objects' do
          expect(described_class[:hello, :world]).to eq [:hello, :world]
        end
      end
    end

    describe '.new' do
      describe 'when initializing with no arguments' do
        it do
          expect(described_class.new).to be_empty
        end
      end

      describe 'when initializing with a size argument' do
        let(:size) { 3 }

        it 'creates an array with size elements set to nil' do
          expect(described_class.new(size)).to eq [nil, nil, nil]
        end

        describe 'when initializing with a default value argument' do
          let(:default_value) { :ruby }

          it 'creates an array with size elements set to the default value' do
            expect(described_class.new(size, default_value)).to eq [:ruby, :ruby, :ruby]
          end
        end

        describe 'when initializing with a block argument' do
          let(:block_argument) { proc { |index| :"ruby#{index}" } }

          it 'creates an array with size elements set to the default value' do
            expect(described_class.new(size, &block_argument)).to eq [:ruby0, :ruby1, :ruby2]
          end
        end
      end

      describe 'when initializing with another array as an argument' do
        let(:other_array) { [:hello, :world] }
        let(:fake_other_array) { double('Fake array', to_ary: other_array) }

        it 'creates a new array' do
          expect(described_class.new(other_array)).to_not be other_array
        end

        it 'creates an array with the same contents as the other array' do
          expect(described_class.new(other_array)).to eq [:hello, :world]
        end

        it 'creates an array with the results of calling #to_ary on the other array' do
          expect(described_class.new(fake_other_array)).to eq [:hello, :world]
        end
      end
    end

    context 'concurrency' do
      it do
        (1..Concurrent::ThreadSafe::Test::THREADS).map do |i|
          in_thread(ary) do |ary|
            1000.times do
              ary << i
              ary.each { |x| x * 2 }
              ary.shift
              ary.last
            end
          end
        end.map(&:join)
        expect(ary).to be_empty
      end
    end

    describe '#slice' do
      # This is mostly relevant on TruffleRuby
      it 'correctly initializes the monitor' do
        ary.concat([0, 1, 2, 3, 4, 5, 6, 7, 8])

        sliced = ary.slice!(0..2)
        expect { sliced[0] }.not_to raise_error
      end
    end
  end
end
