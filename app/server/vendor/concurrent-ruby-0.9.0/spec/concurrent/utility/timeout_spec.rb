module Concurrent

  describe '#timeout', notravis: true do

    it 'raises an exception if no block is given' do
      expect { Concurrent::timeout(0.1) }.to raise_error
    end

    it 'returns the value of the block on success' do
      result = Concurrent::timeout(0.1) { 42 }
      expect(result).to eq 42
    end

    it 'raises an exception if the timeout value is reached' do
      expect { Concurrent::timeout(0.1) { sleep 2 } }.to raise_error(Concurrent::TimeoutError)
    end

    it 'bubbles thread exceptions' do
      expect { Concurrent::timeout(0.1) { raise NotImplementedError } }.to raise_error
    end

    it 'does not kill the thread on timeout' do
      expect(Thread).not_to receive(:kill).with(any_args())
      expect { Concurrent::timeout(0.1) { sleep 2 } }.to raise_error
    end
  end
end
