require 'concurrent/set'
require 'concurrent/atomic/cyclic_barrier'

module Concurrent
  RSpec.describe Set do
    let!(:set) { described_class.new }

    describe '.[]' do
      describe 'when initializing with no arguments' do
        it do
          expect(described_class[]).to be_empty
        end
      end

      describe 'when initializing with arguments' do
        it 'creates a set with the given objects' do
          expect(described_class[:hello, :world]).to eq ::Set.new([:hello, :world])
        end
      end
    end

    describe '.new' do
      describe 'when initializing with no arguments' do
        it do
          expect(described_class.new).to be_empty
        end
      end

      describe 'when initializing with an enumerable object' do
        let(:enumerable_object) { [:hello, :world] }

        it 'creates a set with the contents of the enumerable object' do
          expect(described_class.new(enumerable_object)).to eq ::Set.new([:hello, :world])
        end

        describe 'when initializing with a block argument' do
          let(:block_argument) { proc { |value| :"#{value}_ruby" } }

          it 'creates a set with the contents of the enumerable object' do
            expect(described_class.new(enumerable_object, &block_argument)).to eq ::Set.new([:hello_ruby, :world_ruby])
          end
        end
      end
    end

    context 'concurrency' do
      it '#add and #delete' do
        (1..Concurrent::ThreadSafe::Test::THREADS).map do |i|
          in_thread do
            1000.times do
              v = i
              set << v
              expect(set).not_to be_empty
              set.delete(v)
            end
          end
        end.map(&:join)
        expect(set).to be_empty
      end

      it 'force context switch' do
        barrier = Concurrent::CyclicBarrier.new(2)

        # methods like include? or delete? are implemented for CRuby in Ruby itself
        # @see https://github.com/ruby/ruby/blob/master/lib/set.rb
        set.clear

        # add a single element
        set.add(1)

        # This thread should start and `Set#reject!` in CRuby should cache a value of `0` for size
        thread_reject = in_thread do
          # we expect this to return nil since nothing should have changed.
          expect(set.reject! do |v|
            barrier.wait
            v == 1 # only delete the 1 value
          end).to eq set
        end

        thread_add = in_thread do
          barrier.wait
          expect(set.add?(1)).to eq set
        end

        join_with [thread_reject, thread_add]
      end

      it '#each' do
        threads = []
        ("a".."z").inject(set, &:<<) # setup a non-empty set

        threads << in_thread do
          2000.times do
            size = nil
            set.each do |member|
              if size.nil?
                size = set.length
              else
                expect(set.length).to eq(size)
              end
            end
          end
        end

        threads += (1..19).map do |i|
          in_thread do
            v = i * 1000
            10.times do
              200.times { |j| set << (v+j) }
              200.times { |j| set.delete(v+j) }
            end
          end
        end

        threads.map(&:join)
      end
    end
  end
end
