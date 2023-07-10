require 'concurrent/atomic/atomic_boolean'

RSpec.shared_examples :atomic_boolean do

  describe 'construction' do

    it 'sets the initial value' do
      expect(described_class.new(true).value).to be true
    end

    it 'defaults the initial value to false' do
      expect(described_class.new.value).to be false
    end

    it 'evaluates the truthiness of a true value' do
      expect(described_class.new(10).value).to be true
    end

    it 'evaluates the truthiness of a false value' do
      expect(described_class.new(nil).value).to be false
    end
  end

  describe '#value' do

    it 'returns the current value' do
      counter = described_class.new(true)
      expect(counter.value).to be true
      counter.make_false
      expect(counter.value).to be false
      counter.make_true
      expect(counter.value).to be true
    end
  end

  describe '#value=' do

    it 'sets the #value to the given `Boolean`' do
      atomic = described_class.new(true)
      atomic.value = false
      expect(atomic.value).to be false
    end

    it 'returns the new value' do
      atomic = described_class.new(false)
      expect(atomic.value = true).to be true
    end

    it 'evaluates the truthiness of a true value' do
      atomic = described_class.new(false)
      atomic.value = 10
      expect(atomic.value).to be true
    end

    it 'evaluates the truthiness of a false value' do
      atomic = described_class.new(true)
      atomic.value = nil
      expect(atomic.value).to be false
    end
  end

  describe '#true?' do

    specify { expect(described_class.new(true).true?).to be true }

    specify { expect(described_class.new(false).true?).to be false }
  end

  describe '#false?' do

    specify { expect(described_class.new(true).false?).to be false }

    specify { expect(described_class.new(false).false?).to be true }
  end

  describe '#make_true' do

    it 'makes a false value true and returns true' do
      subject = described_class.new(false)
      expect(subject.make_true).to be true
      expect(subject.value).to be true
    end

    it 'keeps a true value true and returns false' do
      subject = described_class.new(true)
      expect(subject.make_true).to be false
      expect(subject.value).to be true
    end
  end

  describe '#make_false' do

    it 'makes a true value false and returns true' do
      subject = described_class.new(true)
      expect(subject.make_false).to be true
      expect(subject.value).to be false
    end

    it 'keeps a false value false and returns false' do
      subject = described_class.new(false)
      expect(subject.make_false).to be false
      expect(subject.value).to be false
    end
  end
end

module Concurrent

  RSpec.describe MutexAtomicBoolean do

    it_should_behave_like :atomic_boolean

    context 'instance methods' do

      before(:each) do
        expect(subject).to receive(:synchronize).with(no_args).and_return(true)
      end

      specify 'value is synchronized' do
        subject.value
      end

      specify 'value= is synchronized' do
        subject.value = 10
      end

      specify 'true? is synchronized' do
        subject.true?
      end

      specify 'false? is synchronized' do
        subject.false?
      end

      specify 'make_true is synchronized' do
        subject.make_true
      end

      specify 'make_false is synchronized' do
        subject.make_false
      end
    end
  end

  if Concurrent.allow_c_extensions?
    RSpec.describe CAtomicBoolean do
      it_should_behave_like :atomic_boolean
    end
  end

  if Concurrent.on_jruby?
    RSpec.describe JavaAtomicBoolean do
      it_should_behave_like :atomic_boolean
    end
  end

  RSpec.describe AtomicBoolean do
    if RUBY_ENGINE != 'ruby'
      it 'does not load the C extension' do
        expect(defined?(Concurrent::CAtomicBoolean)).to be_falsey
      end
    end

    if Concurrent.on_jruby?
      it 'inherits from JavaAtomicBoolean' do
        expect(AtomicBoolean.ancestors).to include(JavaAtomicBoolean)
      end
    elsif Concurrent.allow_c_extensions?
      it 'inherits from CAtomicBoolean' do
        expect(AtomicBoolean.ancestors).to include(CAtomicBoolean)
      end
    else
      it 'inherits from MutexAtomicBoolean' do
        expect(AtomicBoolean.ancestors).to include(MutexAtomicBoolean)
      end
    end

    describe '#to_s and #inspect' do
      it 'includes the value' do
        subject = described_class.new(true)
        expect(subject.to_s).to include('true')
        expect(subject.inspect).to include('true')
      end
    end
  end
end
