require_relative 'concern/dereferenceable_shared'

module Concurrent

  describe Atom do

    it_should_behave_like :dereferenceable do
      def dereferenceable_subject(value, opts = {})
        Atom.new(value, opts)
      end
    end

    context 'construction' do

      it 'sets the initial value to the given value' do
        atom = Atom.new(42)
        expect(atom.value).to eq 42
      end

      it 'raises an exception if the validator is not a proc' do
        expect {
          Atom.new(42, validator: 42)
        }.to raise_error(ArgumentError)
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

    context 'swap' do

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

      #it 'calls the block more than once if the value changes underneath' do
      #latch = Concurrent::CountDownLatch.new
      #counter = Concurrent::AtomicBoolean.new(0)
      #atom = Atom.new(0)

      #t = Thread.new do
      #atom.swap do |value|
      #counter.increment
      #latch.wait
      #42
      #end
      #end

      #atom.swap{ 100 }
      #latch.count_down
      #t.join(1)

      #expect(counter.value).to eq 2
      #end
    end
  end
end
