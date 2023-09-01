RSpec.shared_examples :collection_each do

  it 'common' do
    @cache.send(method) { |k, v| fail }
    expect(@cache).to eq @cache.send(method) {}
    @cache[:a] = 1

    h = {}
    @cache.send(method) { |k, v| h[k] = v }
    expect({:a => 1}).to eq h

    @cache[:b] = 2
    h = {}
    @cache.send(method) { |k, v| h[k] = v }
    expect({:a => 1, :b => 2}).to eq h
  end

  it 'pair iterator' do
    @cache[:a] = 1
    @cache[:b] = 2
    i = 0
    r = @cache.send(method) do |k, v|
      if i == 0
        i += 1
        next
      elsif i == 1
        break :breaked
      end
    end

    expect(:breaked).to eq r
  end

  it 'allows modification' do
    @cache[:a] = 1
    @cache[:b] = 1
    @cache[:c] = 1

    expect_size_change(1) do
      @cache.send(method) do |k, v|
        @cache[:z] = 1
      end
    end
  end

  context 'when no block is given' do
    it 'returns an enumerator' do
      @cache[:a] = 1
      @cache[:b] = 2

      expect(@cache.send(method)).to be_a Enumerator
    end

    it 'returns an object which is enumerable' do
      @cache[:a] = 1
      @cache[:b] = 2

      expect(@cache.send(method).to_a).to contain_exactly([:a, 1], [:b, 2])
    end
  end
end
