shared_examples_for 'has options' do |object|

  if object.respond_to?(:call)
    subject{ object.call }
  else
    subject{ object }
  end

  describe "dump options" do

    before do
      subject.dump_options = nil
    end

    after do
      subject.dump_options = nil
    end

    it 'returns default options if not set' do
      expect(subject.dump_options).to eq(subject.default_dump_options)
    end

    it 'allows hashes' do
      subject.dump_options = {:foo => 'bar'}
      expect(subject.dump_options).to eq(:foo => 'bar')
    end

    it 'allows objects that implement #to_hash' do
      value = Class.new do
        def to_hash
          {:foo => 'bar'}
        end
      end.new

      subject.dump_options = value
      expect(subject.dump_options).to eq(:foo => 'bar')
    end

    it 'evaluates lambda returning options (with args)' do
      subject.dump_options = lambda{ |a1, a2| { a1 => a2 }}
      expect(subject.dump_options('1', '2')).to eq('1' => '2')
    end

    it 'evaluates lambda returning options (with no args)' do
      subject.dump_options = lambda{{:foo => 'bar'}}
      expect(subject.dump_options).to eq(:foo => 'bar')
    end

    it 'returns empty hash in all other cases' do
      subject.dump_options = true
      expect(subject.dump_options).to eq(subject.default_dump_options)

      subject.dump_options = false
      expect(subject.dump_options).to eq(subject.default_dump_options)

      subject.dump_options = 10
      expect(subject.dump_options).to eq(subject.default_dump_options)

      subject.dump_options = nil
      expect(subject.dump_options).to eq(subject.default_dump_options)
    end
  end

  describe "load options" do

    before do
      subject.load_options = nil
    end

    after do
      subject.load_options = nil
    end

    it 'returns default options if not set' do
      expect(subject.load_options).to eq(subject.default_load_options)
    end

    it 'allows hashes' do
      subject.load_options = {:foo => 'bar'}
      expect(subject.load_options).to eq(:foo => 'bar')
    end

    it 'allows objects that implement #to_hash' do
      value = Class.new do
        def to_hash
          {:foo => 'bar'}
        end
      end.new

      subject.load_options = value
      expect(subject.load_options).to eq(:foo => 'bar')
    end

    it 'evaluates lambda returning options (with args)' do
      subject.load_options = lambda{ |a1, a2| { a1 => a2 }}
      expect(subject.load_options('1', '2')).to eq('1' => '2')
    end

    it 'evaluates lambda returning options (with no args)' do
      subject.load_options = lambda{{:foo => 'bar'}}
      expect(subject.load_options).to eq(:foo => 'bar')
    end

    it 'returns empty hash in all other cases' do
      subject.load_options = true
      expect(subject.load_options).to eq(subject.default_load_options)

      subject.load_options = false
      expect(subject.load_options).to eq(subject.default_load_options)

      subject.load_options = 10
      expect(subject.load_options).to eq(subject.default_load_options)

      subject.load_options = nil
      expect(subject.load_options).to eq(subject.default_load_options)
    end
  end

end
