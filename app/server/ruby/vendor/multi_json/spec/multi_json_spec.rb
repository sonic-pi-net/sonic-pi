require 'spec_helper'
require 'shared/options'

describe MultiJson do
  before(:all) do
    # make sure all available libs are required
    MultiJson::REQUIREMENT_MAP.each do |library, adapter|
      begin
        require library
      rescue ::LoadError
        next
      end
    end
  end

  context 'when no other json implementations are available' do
    around do |example|
      simulate_no_adapters{ example.call }
    end

    it 'defaults to ok_json if no other json implementions are available' do
      silence_warnings do
        expect(MultiJson.default_adapter).to eq(:ok_json)
      end
    end

    it 'prints a warning' do
      expect(Kernel).to receive(:warn).with(/warning/i)
      MultiJson.default_adapter
    end
  end

  context 'caching' do
    before{ MultiJson.use adapter }
    let(:adapter){ MultiJson::Adapters::JsonGem }
    let(:json_string){ '{"abc":"def"}' }

    it 'busts caches on global options change' do
      MultiJson.load_options = { :symbolize_keys => true }
      expect(MultiJson.load(json_string)).to eq(:abc => 'def')
      MultiJson.load_options = nil
      expect(MultiJson.load(json_string)).to eq('abc' => 'def')
    end

    it 'busts caches on per-adapter options change' do
      adapter.load_options = { :symbolize_keys => true }
      expect(MultiJson.load(json_string)).to eq(:abc => 'def')
      adapter.load_options = nil
      expect(MultiJson.load(json_string)).to eq('abc' => 'def')
    end
  end

  it 'defaults to the best available gem' do
    # Clear cache variable already set by previous tests
    MultiJson.send(:remove_instance_variable, :@adapter) if MultiJson.instance_variable_defined?(:@adapter)

    if jruby?
      expect(MultiJson.adapter.to_s).to eq('MultiJson::Adapters::JrJackson')
    else
      expect(MultiJson.adapter.to_s).to eq('MultiJson::Adapters::Oj')
    end
  end

  it 'looks for adapter even if @adapter variable is nil' do
    MultiJson.send(:instance_variable_set, :@adapter, nil)
    expect(MultiJson).to receive(:default_adapter).and_return(:ok_json)
    expect(MultiJson.adapter).to eq(MultiJson::Adapters::OkJson)
  end

  it 'is settable via a symbol' do
    MultiJson.use :json_gem
    expect(MultiJson.adapter).to eq(MultiJson::Adapters::JsonGem)
  end

  it 'is settable via a case-insensitive string' do
    MultiJson.use 'Json_Gem'
    expect(MultiJson.adapter).to eq(MultiJson::Adapters::JsonGem)
  end

  it 'is settable via a class' do
    adapter = Class.new
    MultiJson.use adapter
    expect(MultiJson.adapter).to eq(adapter)
  end

  it 'is settable via a module' do
    adapter = Module.new
    MultiJson.use adapter
    expect(MultiJson.adapter).to eq(adapter)
  end

  it 'throws AdapterError on bad input' do
    expect{ MultiJson.use 'bad adapter' }.to raise_error(MultiJson::AdapterError, /bad adapter/)
  end

  it 'gives access to original error when raising AdapterError' do
    exception = get_exception(MultiJson::AdapterError){ MultiJson.use 'foobar' }
    expect(exception.cause).to be_instance_of(::LoadError)
    expect(exception.message).to include("-- multi_json/adapters/foobar")
    expect(exception.message).to include("Did not recognize your adapter specification")
  end

  context 'using one-shot parser' do
    before do
      expect(MultiJson::Adapters::JsonPure).to receive(:dump).once.and_return('dump_something')
      expect(MultiJson::Adapters::JsonPure).to receive(:load).once.and_return('load_something')
    end

    it 'should use the defined parser just for the call' do
      MultiJson.use :json_gem
      expect(MultiJson.dump('', :adapter => :json_pure)).to eq('dump_something')
      expect(MultiJson.load('', :adapter => :json_pure)).to eq('load_something')
      expect(MultiJson.adapter).to eq(MultiJson::Adapters::JsonGem)
    end
  end

  it 'can set adapter for a block' do
    MultiJson.use :ok_json
    MultiJson.with_adapter(:json_pure) do
      MultiJson.with_engine(:json_gem) do
        expect(MultiJson.adapter).to eq(MultiJson::Adapters::JsonGem)
      end
      expect(MultiJson.adapter).to eq(MultiJson::Adapters::JsonPure)
    end
    expect(MultiJson.adapter).to eq(MultiJson::Adapters::OkJson)
  end

  it 'JSON gem does not create symbols on parse' do
    MultiJson.with_engine(:json_gem) do
      MultiJson.load('{"json_class":"ZOMG"}') rescue nil

      expect{
        MultiJson.load('{"json_class":"OMG"}') rescue nil
      }.to_not change{Symbol.all_symbols.count}
    end
  end

  unless jruby?
    it 'Oj does not create symbols on parse' do
      MultiJson.with_engine(:oj) do
        MultiJson.load('{"json_class":"ZOMG"}') rescue nil

        expect{
          MultiJson.load('{"json_class":"OMG"}') rescue nil
        }.to_not change{Symbol.all_symbols.count}
      end
    end

    context 'with Oj.default_settings' do
      around do |example|
        options = Oj.default_options
        Oj.default_options = { :symbol_keys => true }
        MultiJson.with_engine(:oj){ example.call }
        Oj.default_options = options
      end

      it 'ignores global settings' do
        MultiJson.with_engine(:oj) do
          example = '{"a": 1, "b": 2}'
          expected = { 'a' => 1, 'b' => 2 }
          expect(MultiJson.load(example)).to eq(expected)
        end
      end
    end
  end

  describe 'default options' do
    after(:all){ MultiJson.load_options = MultiJson.dump_options = nil }

    it 'is deprecated' do
      expect(Kernel).to receive(:warn).with(/deprecated/i)
      silence_warnings{ MultiJson.default_options = {:foo => 'bar'} }
    end

    it 'sets both load and dump options' do
      expect(MultiJson).to receive(:dump_options=).with(:foo => 'bar')
      expect(MultiJson).to receive(:load_options=).with(:foo => 'bar')
      silence_warnings{ MultiJson.default_options = {:foo => 'bar'} }
    end
  end

  it_behaves_like 'has options', MultiJson

  describe 'aliases' do
    if jruby?
      describe 'jrjackson' do
        after{ expect(MultiJson.adapter).to eq(MultiJson::Adapters::JrJackson) }

        it 'allows jrjackson alias as symbol' do
          expect{ MultiJson.use :jrjackson }.not_to raise_error
        end

        it 'allows jrjackson alias as string' do
          expect{ MultiJson.use 'jrjackson' }.not_to raise_error
        end

      end
    end
  end
end
