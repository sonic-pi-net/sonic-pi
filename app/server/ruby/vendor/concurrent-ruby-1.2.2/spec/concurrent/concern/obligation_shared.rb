RSpec.shared_examples :obligation do

  context '#state' do

    it 'is :pending when first created' do
      f = pending_subject
      expect(f.state).to eq(:pending)
      expect(f).to be_pending
    end

    it 'is :fulfilled when the handler completes' do
      f = fulfilled_subject
      expect(f.state).to eq(:fulfilled)
      expect(f).to be_fulfilled
    end

    it 'is :rejected when the handler raises an exception' do
      f = rejected_subject
      expect(f.state).to eq(:rejected)
      expect(f).to be_rejected
    end
  end

  context '#value' do

    let!(:supports_timeout) { pending_subject.method(:value).arity != 0 }

    it 'returns nil when reaching the optional timeout value' do
      if supports_timeout
        f = pending_subject
        expect(f.value(0)).to be_nil
        expect(f).to be_pending
      end
    end

    it 'returns immediately when timeout is zero' do
      if supports_timeout
        expect(Concurrent).not_to receive(:timeout).with(any_args())
        f = pending_subject
        expect(f.value(0)).to be_nil
        expect(f).to be_pending
      end
    end

    it 'returns the value when fulfilled before timeout' do
      if supports_timeout
        f = pending_subject
        expect(f.value(10)).to be_truthy
        expect(f).to be_fulfilled
      end
    end

    it 'returns nil when timeout reached' do
      if supports_timeout
        f = pending_subject
        expect(f.value(0.001)).to be_nil
        expect(f).to be_pending
      end
    end

    it 'is nil when :pending' do
      if supports_timeout
        expected = pending_subject.value(0)
        expect(expected).to be_nil
      end
    end

    it 'blocks the caller when :pending and timeout is nil' do
      f = pending_subject
      expect(f.value).to be_truthy
      expect(f).to be_fulfilled
    end

    it 'is nil when :rejected' do
      expected = rejected_subject.value
      expect(expected).to be_nil
    end

    it 'is set to the return value of the block when :fulfilled' do
      expected = fulfilled_subject.value
      expect(expected).to eq fulfilled_value
    end
  end

  context '#reason' do

    it 'is nil when :pending' do
      expect(pending_subject.reason).to be_nil
    end

    it 'is nil when :fulfilled' do
      expect(fulfilled_subject.reason).to be_nil
    end

    it 'is set to error object of the exception when :rejected' do
      expect(rejected_subject.reason).to be_a(Exception)
      expect(rejected_subject.reason.to_s).to match(/#{rejected_reason}/)
    end
  end
end
