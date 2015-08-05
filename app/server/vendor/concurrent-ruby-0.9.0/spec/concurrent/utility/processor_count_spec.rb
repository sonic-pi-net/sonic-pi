module Concurrent

  describe '#processor_count' do

    it 'retuns a positive integer' do
      expect(Concurrent::processor_count).to be_a Integer
      expect(Concurrent::processor_count).to be >= 1
    end
  end

  describe '#physical_processor_count' do

    it 'retuns a positive integer' do
      expect(Concurrent::physical_processor_count).to be_a Integer
      expect(Concurrent::physical_processor_count).to be >= 1
    end
  end
end
