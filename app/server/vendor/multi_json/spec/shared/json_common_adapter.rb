shared_examples_for 'JSON-like adapter' do |adapter|
  before{ MultiJson.use adapter }

  describe '.dump' do
    before{ MultiJson.dump_options = MultiJson.adapter.dump_options = nil }

    describe 'with :pretty option set to true' do
      it 'passes default pretty options' do
        object = 'foo'
        expect(object).to receive(:to_json).with(JSON::PRETTY_STATE_PROTOTYPE.to_h)
        MultiJson.dump(object, :pretty => true)
      end
    end

    describe 'with :indent option' do
      it 'passes it on dump' do
        object = 'foo'
        expect(object).to receive(:to_json).with(:indent => "\t")
        MultiJson.dump(object, :indent => "\t")
      end
    end
  end

  describe '.load' do
    it 'passes :quirks_mode option' do
      expect(::JSON).to receive(:parse).with('[123]', {:quirks_mode => false, :create_additions => false})
      MultiJson.load('[123]', :quirks_mode => false)
    end
  end
end
