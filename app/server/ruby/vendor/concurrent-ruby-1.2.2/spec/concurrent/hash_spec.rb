require 'concurrent/hash'

module Concurrent
  RSpec.describe Hash do
    let!(:hsh) { described_class.new }

    describe '.[]' do
      describe 'when initializing with no arguments' do
        it do
          expect(described_class[]).to be_empty
        end
      end

      describe 'when initializing with an even number of arguments' do
        it 'creates a hash using the odd position arguments as keys and even position arguments as values' do
          expect(described_class[:hello, 'hello', :world, 'world']).to eq(hello: 'hello', world: 'world')
        end
      end

      describe 'when initializing with an array of pairs' do
        let(:array_of_pairs) { [[:hello, 'hello'], [:world, 'world']] }

        it 'creates a hash using each pair as a (key, value) pair' do
          expect(described_class[array_of_pairs]).to eq(hello: 'hello', world: 'world')
        end
      end

      describe 'when initializing with another hash as an argument' do
        let(:other_hash) { {hello: 'hello', world: 'world'} }
        let(:fake_other_hash) { double('Fake hash', to_hash: other_hash) }

        it 'creates a new hash' do
          expect(described_class[other_hash]).to_not be other_hash
        end

        it 'creates a hash with the same contents as the other hash' do
          expect(described_class[other_hash]).to eq(hello: 'hello', world: 'world')
        end

        it 'creates a hash with the results of calling #to_hash on the other array' do
          expect(described_class[fake_other_hash]).to eq(hello: 'hello', world: 'world')
        end
      end
    end

    describe '.new' do
      describe 'when initializing with no arguments' do
        it do
          expect(described_class.new).to be_empty
        end
      end

      describe 'when initialized with a default object' do
        let(:default_object) { :ruby }

        it 'uses the default object for non-existing keys' do
          hash = described_class.new(default_object)

          expect(hash[:hello]).to be :ruby
          expect(hash[:world]).to be :ruby
        end
      end

      describe 'when initialized with a block' do
        it 'calls the block for non-existing keys' do
          block_calls = []

          hash = described_class.new do |hash_instance, key|
            block_calls << [hash_instance, key]
          end

          hash[:hello]
          hash[:world]

          expect(block_calls).to eq [[hash, :hello], [hash, :world]]
        end

        it 'returns the results of calling the block for non-existing key' do
          block_results = ['hello', 'world']

          hash = described_class.new do
            block_results.shift
          end

          expect(hash[:hello]).to eq 'hello'
          expect(hash[:world]).to eq 'world'
        end
      end
    end

    context 'concurrency' do
      it do
        (1..Concurrent::ThreadSafe::Test::THREADS).map do |i|
          in_thread do
            1000.times do |j|
              hsh[i * 1000 + j] = i
              expect(hsh[i * 1000 + j]).to eq(i)
              expect(hsh.delete(i * 1000 + j)).to eq(i)
            end
          end
        end.map(&:join)
        expect(hsh).to be_empty
      end
    end
  end
end
