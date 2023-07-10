require_relative 'concern/observable_shared'
require 'concurrent/atom'
require 'concurrent/atomic/count_down_latch'
require 'concurrent/atomic/atomic_fixnum'

module Concurrent

  RSpec.describe Atom do

    context 'construction' do

      it 'sets the initial value to the given value' do
        atom = Atom.new(42)
        expect(atom.value).to eq 42
      end
    end

    context '#compare_and_set' do

      it 'sets the new value if the current value matches' do
        atom = Atom.new(42)
        atom.compare_and_set(42, :foo)
        expect(atom.value).to eq :foo
      end

      it 'returns true if the current value matches' do
        atom = Atom.new(42)
        expect(atom.compare_and_set(42, :foo)).to be true
      end

      it 'rejects the new value if the current value does not match' do
        atom = Atom.new(42)
        atom.compare_and_set(:foo, 'bar')
        expect(atom.value).to eq 42
      end

      it 'returns false if the current value does not match' do
        atom = Atom.new(42)
        expect(atom.compare_and_set(:foo, 'bar')).to be false
      end

      it 'rejects the new value if the validator returns false' do
        validator = ->(value){ false }
        atom = Atom.new(42, validator: validator)
        atom.compare_and_set(42, :foo)
        expect(atom.value).to eq 42
      end

      it 'rejects the new value if the validator raises an exception' do
        validator = ->(value){ raise StandardError }
        atom = Atom.new(42, validator: validator)
        atom.compare_and_set(42, :foo)
        expect(atom.value).to eq 42
      end

      it 'returns false if the validator returns false' do
        validator = ->(value){ false }
        atom = Atom.new(42, validator: validator)
        expect(atom.compare_and_set(42, :foo)).to be false
      end

      it 'returns false if the validator raises an exception' do
        validator = ->(value){ raise StandardError }
        atom = Atom.new(42, validator: validator)
        expect(atom.compare_and_set(42, :foo)).to be false
      end
    end

    context '#swap' do

      it 'raises an exception when no block is given' do
        atom = Atom.new(42)
        expect {
          atom.swap
        }.to raise_error(ArgumentError)
      end

      it 'passes the current value to the block' do
        actual = nil
        expected = 42
        atom = Atom.new(expected)
        atom.swap do |value|
          actual = value
        end
        expect(actual).to eq expected
      end

      it 'passes all arguments to the block' do
        actual = nil
        expected = [1, 2, 3]
        atom = Atom.new(42)
        atom.swap(*expected) do |value, *args|
          actual = args
        end
        expect(actual).to eq expected
      end

      it 'sets the new value to the result of the block' do
        atom = Atom.new(42)
        atom.swap{ :foo }
        expect(atom.value).to eq :foo
      end

      it 'rejects the new value if the validator returns false' do
        validator = ->(value){ false }
        atom = Atom.new(42, validator: validator)
        atom.swap{ 100 }
        expect(atom.value).to eq 42
      end

      it 'rejects the new value if the validator raises an exception' do
        validator = ->(value){ raise StandardError }
        atom = Atom.new(42, validator: validator)
        atom.swap{ 100 }
        expect(atom.value).to eq 42
      end

      it 'returns the new value on success' do
        atom = Atom.new(42)
        expect(atom.swap{ :foo }).to eq :foo
      end

      it 'returns the old value if the validator returns false' do
        validator = ->(value){ false }
        atom = Atom.new(42, validator: validator)
        expect(atom.swap{ 100 }).to eq 42
      end

      it 'returns the old value if the validator raises an exception' do
        validator = ->(value){ raise StandardError }
        atom = Atom.new(42, validator: validator)
        expect(atom.swap{ 100 }).to eq 42
      end

      it 'calls the block more than once if the value changes underneath' do
        latch1 = Concurrent::CountDownLatch.new
        latch2 = Concurrent::CountDownLatch.new
        counter = Concurrent::AtomicFixnum.new(0)
        atom = Atom.new(0)

        t = in_thread do
          atom.swap do |value|
            latch1.count_down
            latch2.wait(1)
            counter.increment
            42
          end
        end

        latch1.wait(1)
        atom.swap{ 100 }
        latch2.count_down
        t.join(1)

        expect(counter.value).to be > 1
      end

      it 'reraises the exception from block' do
        atom = Atom.new(0)
        expect do
          atom.swap do |value|
            fail 'something went wrong'
          end
        end.to raise_error 'something went wrong'
      end
    end

    context '#reset' do

      it 'sets the new value' do
        atom = Atom.new(42)
        atom.reset(:foo)
        expect(atom.value).to eq :foo
      end

      it 'returns the new value on success' do
        atom = Atom.new(42)
        expect(atom.reset(:foo)).to eq :foo
      end

      it 'returns the new value on success' do
        atom = Atom.new(42)
        expect(atom.reset(:foo)).to eq :foo
      end

      it 'returns the old value if the validator returns false' do
        validator = ->(value){ false }
        atom = Atom.new(42, validator: validator)
        expect(atom.reset(:foo)).to eq 42
      end

      it 'returns the old value if the validator raises an exception' do
        validator = ->(value){ raise StandardError }
        atom = Atom.new(42, validator: validator)
        expect(atom.reset(:foo)).to eq 42
      end
    end

    context :observable do

      subject { Atom.new(0) }

      def trigger_observable(observable)
        observable.reset(42)
      end

      it_behaves_like :observable
    end
  end
end
