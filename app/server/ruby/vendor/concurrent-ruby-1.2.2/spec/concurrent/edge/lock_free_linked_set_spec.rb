require 'concurrent/edge/lock_free_linked_set'
require 'securerandom'

RSpec.describe Concurrent::Edge::LockFreeLinkedSet, edge: true do
  subject { described_class.new }

  describe '.new' do
    context 'when passed default val' do
      it 'uses the val arg as data for each node' do
        set = described_class.new 3, true
        expect(set.all? { |val| val == true }).to be_truthy
      end
    end
  end

  describe '#add' do
    it 'appends to the linked set' do
      expect(subject.add 'test string1').to be true
    end

    context 'in a multi-threaded environment' do
      it 'adds the items to the set' do
        to_insert = %w(one two three four five six)

        threads = ::Array.new(16) do
          in_thread do
            to_insert.each do |item|
              subject.add item
            end
          end
        end

        threads.each(&:join)

        to_insert.each do |item|
          expect(subject.contains? item).to be true
        end
      end
    end
  end

  describe '#<<' do
    it 'appends to the linked set and returns self' do
      expect(subject << 'test string1').to be_a described_class
    end

    it 'returns self regardless of whether it was logically added' do
      subject << 'test string'
      expect(subject << 'test string').to be_a described_class
    end
  end

  describe '#contains?' do
    context 'when checking if set includes a value' do
      it 'returns true if a value exists' do
        subject << 'Concurrency... ooh! ahh!'
        expect(subject.contains? 'Concurrency... ooh! ahh!').to eq true
      end

      it 'compares object using Object#hash' do
        val = 'Hash me.'
        subject << val
        expect(subject.contains? 'Hash me.').to eq true
      end

      it 'returns false for values not in the set' do
        subject << 'Concurrency... ooh! ahh!'
        expect(subject.contains? 'Sequential... booh! nah!').to eq false
      end

      context 'when set is empty' do
        it 'does not break' do
          expect(subject.contains? 'Nothing to see here.').to eq false
        end
      end

      context 'when set is long' do
        it 'does not break' do
          arr = ::Array.new(1000) { SecureRandom.hex }
          arr.each { |n| subject << n }
          ret = arr.all? { |n| subject.contains? n }

          expect(ret).to be true
        end
      end

      context 'in a multi-threaded environment' do
        it 'correctly check that the set contains the item' do
          to_insert = %w(one two three four five six)
          to_insert.each { |item| subject << item }

          threads = ::Array.new(16) do
            in_thread do
              100.times { subject << SecureRandom.hex }

              to_insert.each do |item|
                expect(subject.contains? item).to be true
              end
            end
          end

          threads.each(&:join)

          to_insert.each do |item|
            expect(subject.contains? item).to be true
          end
        end
      end
    end
  end

  describe '#remove' do
    context 'when item is inside of set' do
      before { subject << 'one' << 'two' << 'three' }

      it 'the item is no longer visible to the user' do
        subject.remove 'three'

        expect(subject.contains? 'three').to be false
      end

      it 'allows for the item to be added despite being physically present' do
        subject.remove 'three'

        expect(subject.add 'three').to be true
      end
    end

    context 'in a multi-threaded environment' do

      it 'adds the items to the set' do
        to_insert = %w(one two three four five six)
        to_insert.each { |item| subject << item }

        threads = ::Array.new(8) do
          [in_thread { subject.remove 'one' },
           in_thread { subject.remove 'two' },
           in_thread { subject.remove 'three' }]
        end

        threads.flatten.each(&:join)

        expect(subject.contains? 'one').to be false
        expect(subject.contains? 'two').to be false
        expect(subject.contains? 'three').to be false
        expect(subject.contains? 'four').to be true
        expect(subject.contains? 'five').to be true
        expect(subject.contains? 'six').to be true
      end

      it 'does not recognize the existence of the item when removed' do
        to_insert = %w(one two three four five six)
        to_insert.each { |item| subject << item }

        ::Array.new(16) do
          in_thread do
            100.times { subject << SecureRandom.hex }

            to_insert.each do |item|
              subject.remove item
              expect(subject.contains? item).to be false
            end
          end
        end
      end
    end

    context 'when item is not inside of set' do
      before { subject << 'one' << 'two' << 'three' }

      it 'does not remove to value' do
        expect(subject.remove 'four').to be false
      end

      it 'the set remains intact' do
        expect(subject).to receive :remove
        subject.remove 'four'

        present = %w(one two three).map { |n| subject.contains? n }.all?
        expect(present).to be true
      end

      context 'when the set is empty' do
        subject { described_class.new }

        it 'remove does not break' do
          expect(subject.remove 'test').to be false
        end
      end

      context 'when the set is large' do
        subject { described_class.new(1000) { SecureRandom.hex } }

        it 'remove successfully removes the node' do
          subject << 'Testing'
          expect(subject.remove 'Testing').to be true
        end
      end
    end
  end
end
