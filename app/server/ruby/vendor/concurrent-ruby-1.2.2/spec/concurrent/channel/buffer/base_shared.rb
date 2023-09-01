RSpec.shared_examples :channel_buffer do

  specify do
    expect(subject).to respond_to(:blocking?)
  end

  context '#capacity' do
    specify { expect(subject.capacity).to be >= 0 }
  end

  context '#size' do
    it 'returns zero upon initialization' do
      expect(subject.size).to eq 0
    end
  end

  context '#empty?' do
    it 'returns true when empty' do
      expect(subject).to be_empty
    end
  end

  context '#full?' do
    it 'returns false when not full' do
      expect(subject).to_not be_full
    end
  end

  context '#put' do

    it 'does not enqueue the item when closed' do
      subject.close
      subject.put(:foo)
      expect(subject).to be_empty
    end

    it 'returns false when closed' do
      subject.close
      expect(subject.put(:foo)).to be false
    end
  end

  context '#offer' do

    it 'returns true on success' do
      subject # initialize on this thread
      t = in_thread do
        subject.take
      end
      t.join(0.1)

      expect(subject.offer(:foo)).to be true
    end

    it 'does not enqueue the item when closed' do
      subject.close
      subject.offer(:foo)
      expect(subject).to be_empty
    end

    it 'returns false immediately when closed' do
      subject.close
      expect(subject.offer(:foo)).to be false
    end
  end

  context '#take' do
    it 'returns Concurrent::NULL when closed' do
      subject.close
      expect(subject.take).to eq Concurrent::NULL
    end
  end

  context '#next' do
    it 'returns Concurrent::NULL, false when closed' do
      subject.close
      item, more = subject.next
      expect(item).to eq Concurrent::NULL
      expect(more).to be false
    end
  end

  context '#poll' do

    it 'returns the next item immediately if available' do
      subject # initialize on this thread
      t = in_thread do
        subject.put(42)
      end
      t.join(0.1)

      # TODO (pitr-ch 15-Oct-2016): fails on JRuby https://travis-ci.org/pitr-ch/concurrent-ruby/jobs/167937038
      expect(subject.poll).to eq 42
    end

    it 'returns Concurrent::NULL immediately if no item is available' do
      expect(subject.poll).to eq Concurrent::NULL
    end

    it 'returns Concurrent::NULL when closed' do
      subject.close
      expect(subject.poll).to eq Concurrent::NULL
    end
  end

  context '#close' do

    it 'sets #closed? to false' do
      subject.close
      expect(subject).to be_closed
    end

    it 'returns true when not previously closed' do
      expect(subject.close).to be true
    end

    it 'returns false when already closed' do
      subject.close
      expect(subject.close).to be false
    end
  end

  context '#closed?' do

    it 'returns true when new' do
      expect(subject).to_not be_closed
    end

    it 'returns false after #close' do
      subject.close
      expect(subject).to be_closed
    end
  end
end
