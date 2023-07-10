require 'concurrent/collection/non_concurrent_priority_queue'

RSpec.shared_examples :priority_queue do

  subject{ described_class.new }

  context '#initialize' do

    it 'sorts from high to low when :order is :max' do
      subject = described_class.from_list([2, 1, 4, 5, 3, 0], order: :max)
      expect(subject.pop).to eq 5
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
    end

    it 'sorts from high to low when :order is :high' do
      subject = described_class.new(order: :high)
      [2, 1, 4, 5, 3, 0].each{|item| subject << item }
      expect(subject.pop).to eq 5
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
    end

    it 'sorts from low to high when :order is :min' do
      subject = described_class.from_list([2, 1, 4, 5, 3, 0], order: :min)
      expect(subject.pop).to eq 0
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 2
    end

    it 'sorts from low to high when :order is :low' do
      subject = described_class.new(order: :low)
      [2, 1, 4, 5, 3, 0].each{|item| subject << item }
      expect(subject.pop).to eq 0
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 2
    end

    it 'sorts from high to low by default' do
      subject = described_class.new
      subject = described_class.from_list([2, 1, 4, 5, 3, 0])
      expect(subject.pop).to eq 5
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
    end
  end

  context '#clear' do

    it 'removes all items from a populated queue' do
      10.times{|i| subject << i}
      subject.clear
      expect(subject).to be_empty
    end

    it 'has no effect on an empty queue' do
      subject.clear
      expect(subject).to be_empty
    end

    specify { expect(subject.clear).to be_truthy }
  end

  context '#delete' do

    it 'deletes the requested item when found' do
      10.times{|item| subject << item }
      subject.delete(5)
      expect(subject.pop).to eq 9
      expect(subject.pop).to eq 8
      expect(subject.pop).to eq 7
      expect(subject.pop).to eq 6
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
      expect(subject.pop).to eq 2
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 0
    end

    it 'deletes the requested item when it is the first element' do
      10.times{|item| subject << item }
      subject.delete(9)
      expect(subject.length).to eq 9
      expect(subject.pop).to eq 8
      expect(subject.pop).to eq 7
      expect(subject.pop).to eq 6
      expect(subject.pop).to eq 5
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
      expect(subject.pop).to eq 2
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 0
    end

    it 'deletes the requested item when it is the last element' do
      10.times{|item| subject << item }
      subject.delete(2)
      expect(subject.length).to eq 9
      expect(subject.pop).to eq 9
      expect(subject.pop).to eq 8
      expect(subject.pop).to eq 7
      expect(subject.pop).to eq 6
      expect(subject.pop).to eq 5
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 0
    end

    it 'deletes multiple matching items when present' do
      [2, 1, 2, 2, 2, 3, 2].each{|item| subject << item }
      subject.delete(2)
      expect(subject.pop).to eq 3
      expect(subject.pop).to eq 1
    end

    it 'returns true when found' do
      10.times{|i| subject << i}
      expect(subject.delete(2)).to be_truthy
    end

    it 'returns false when not found' do
      10.times{|i| subject << i}
      expect(subject.delete(100)).to be_falsey
    end

    it 'returns false when called on an empty queue' do
      expect(subject.delete(:foo)).to be_falsey
    end

    def dequeue_all(queue)
      queue.size.times.inject([]) do |acc, _|
        acc << queue.pop
      end
    end

    it 'deletes the requested item when it is "smaller" than the last element' do
      [
        100,
        9, 90,
        7, 8, 70, 80,
        3, 4, 5, 6, 30, 40, 50, 60
      ].each do |item|
        subject << item
      end

      subject.delete(8)

      expect(subject.length).to eq 14
      expect(subject.pop).to eq 100
      expect(subject.pop).to eq 90
      expect(subject.pop).to eq 80
      expect(subject.pop).to eq 70
      expect(subject.pop).to eq 60
      expect(subject.pop).to eq 50
      expect(subject.pop).to eq 40
      expect(subject.pop).to eq 30
      expect(subject.pop).to eq 9
      expect(subject.pop).to eq 7
      expect(subject.pop).to eq 6
      expect(subject.pop).to eq 5
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
    end
  end

  context '#empty?' do

    it 'returns true for an empty queue' do
      expect(subject).to be_empty
    end

    it 'returns false for a populated queue' do
      10.times{|i| subject << i}
      expect(subject).not_to be_empty
    end
  end

  context '#include?' do

    it 'returns true if the item is found' do
      10.times{|i| subject << i}
      expect(subject).to include(5)
    end

    it 'returns false if the item is not found' do
      10.times{|i| subject << i}
      expect(subject).not_to include(50)
    end

    it 'returns false when the queue is empty' do
      expect(subject).not_to include(1)
    end

    it 'is aliased as #has_priority?' do
      10.times{|i| subject << i}
      expect(subject).to have_priority(5)
    end
  end

  context '#length' do

    it 'returns the length of a populated queue' do
      10.times{|i| subject << i}
      expect(subject.length).to eq 10
    end

    it 'returns zero when the queue is empty' do
      expect(subject.length).to eq 0
    end

    it 'is aliased as #size' do
      10.times{|i| subject << i}
      expect(subject.size).to eq 10
    end
  end

  context '#peek' do

    it 'returns the item at the head of the queue' do
      10.times{|i| subject << i}
      expect(subject.peek).to eq 9
    end

    it 'does not remove the item from the queue' do
      10.times{|i| subject << i}
      subject.peek
      expect(subject.length).to eq 10
      expect(subject).to include(9)
    end

    it 'returns nil when the queue is empty' do
      expect(subject.peek).to be_nil
    end
  end

  context '#pop' do

    it 'returns the item at the head of the queue' do
      10.times{|i| subject << i}
      expect(subject.pop).to eq 9
    end

    it 'removes the item from the queue' do
      10.times{|i| subject << i}
      subject.pop
      expect(subject.length).to eq 9
      expect(subject).not_to include(9)
    end

    it 'returns nil when the queue is empty' do
      expect(subject.pop).to be_nil
    end

    it 'returns nil when called multiple times while empty' do
      10.times do
        expect(subject.pop).to be nil
      end
    end

    it 'is aliased as #deq' do
      10.times{|i| subject << i}
      expect(subject.deq).to eq 9
    end

    it 'is aliased as #shift' do
      10.times{|i| subject << i}
      expect(subject.shift).to eq 9
    end
  end

  context '#push' do

    it 'raises an exception when attempting to enqueue nil' do
      expect {
        subject.push(nil)
      }.to raise_error(ArgumentError)
    end

    it 'adds the item to the queue' do
      subject.push(1)
      expect(subject).to include(1)
    end

    it 'sorts the new item in priority order' do
      3.times{|i| subject << i}
      expect(subject.pop).to eq 2
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 0
    end

    it 'arbitrarily orders equal items with respect to each other' do
      3.times{|i| subject << i}
      subject.push(1)
      expect(subject.pop).to eq 2
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 0
    end

    specify { expect(subject.push(10)).to be_truthy }

    it 'is aliased as <<' do
      subject << 1
      expect(subject).to include(1)
    end

    it 'is aliased as enq' do
      subject.enq(1)
      expect(subject).to include(1)
    end
  end

  context '.from_list' do

    it 'creates an empty queue from an empty list' do
      subject = described_class.from_list([])
      expect(subject).to be_empty
    end

    it 'creates a sorted, populated queue from an Array' do
      subject = described_class.from_list([2, 1, 4, 5, 3, 0])
      expect(subject.pop).to eq 5
      expect(subject.pop).to eq 4
      expect(subject.pop).to eq 3
      expect(subject.pop).to eq 2
      expect(subject.pop).to eq 1
      expect(subject.pop).to eq 0
    end

    it 'creates a sorted, populated queue from a Hash' do
      subject = described_class.from_list(two: 2, one: 1, three: 3, zero: 0)
      expect(subject.length).to eq 4
    end
  end
end

module Concurrent
  module Collection

    RSpec.describe RubyNonConcurrentPriorityQueue do

      it_should_behave_like :priority_queue
    end

    if Concurrent.on_jruby?

      RSpec.describe JavaNonConcurrentPriorityQueue do

        it_should_behave_like :priority_queue
      end
    end

    RSpec.describe NonConcurrentPriorityQueue do
      if Concurrent.on_jruby?
        it 'inherits from JavaNonConcurrentPriorityQueue' do
          expect(NonConcurrentPriorityQueue.ancestors).to include(JavaNonConcurrentPriorityQueue)
        end
      else
        it 'inherits from RubyNonConcurrentPriorityQueue' do
          expect(NonConcurrentPriorityQueue.ancestors).to include(RubyNonConcurrentPriorityQueue)
        end
      end
    end
  end
end
