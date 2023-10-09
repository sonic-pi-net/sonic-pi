require 'concurrent/atomic/atomic_fixnum'

RSpec.shared_examples :atomic_fixnum do

  context 'construction' do

    it 'sets the initial value' do
      expect(described_class.new(10).value).to eq 10
    end

    it 'defaults the initial value to zero' do
      expect(described_class.new.value).to eq 0
    end

    it 'raises an exception if the initial value is not a Fixnum' do
      expect { described_class.new(10.01) }.to(raise_error { |error|
        expect(error.class).to be(ArgumentError).or(be(TypeError))
      })
    end
  end

  context '#value' do

    it 'returns the current value' do
      counter = described_class.new(10)
      expect(counter.value).to eq 10
      counter.increment
      expect(counter.value).to eq 11
      counter.decrement
      expect(counter.value).to eq 10
    end
  end

  context '#value=' do

    it 'sets the #value to the given `Fixnum`' do
      atomic = described_class.new(0)
      atomic.value = 10
      expect(atomic.value).to eq 10
    end

    it 'returns the new value' do
      atomic = described_class.new(0)
      expect(atomic.value = 10).to eq 10
    end

    it 'raises and exception if the value is not a `Fixnum`' do
      atomic = described_class.new(0)
      expect {
        atomic.value = 'foo'
      }.to(raise_error { |error|
        expect(error.class).to be(ArgumentError).or(be(TypeError))
      })
    end
  end

  context '#increment' do

    it 'increases the value by one when no argument is given' do
      counter = described_class.new(10)
      3.times{ counter.increment }
      expect(counter.value).to eq 13
    end

    it 'returns the new value when no argument is given' do
      counter = described_class.new(10)
      expect(counter.increment).to eq 11
    end

    it 'increases the value by the given argument' do
      counter = described_class.new(10)
      counter.increment(5)
      expect(counter.value).to eq 15
    end

    it 'returns the new value the given argument' do
      counter = described_class.new(10)
      expect(counter.increment(5)).to eq 15
    end

    it 'is aliased as #up' do
      expect(described_class.new(10).up).to eq 11
    end
  end

  context '#decrement' do

    it 'decreases the value by one when no argument is given' do
      counter = described_class.new(10)
      3.times{ counter.decrement }
      expect(counter.value).to eq 7
    end

    it 'returns the new value when no argument is given' do
      counter = described_class.new(10)
      expect(counter.decrement).to eq 9
    end

    it 'decreases the value by the given argument' do
      counter = described_class.new(10)
      counter.decrement(5)
      expect(counter.value).to eq 5
    end

    it 'returns the new value the given argument' do
      counter = described_class.new(10)
      expect(counter.decrement(5)).to eq 5
    end

    it 'is aliased as #down' do
      expect(described_class.new(10).down).to eq 9
    end
  end

  context '#compare_and_set' do

    it 'returns false if the value is not found' do
      expect(described_class.new(14).compare_and_set(2, 14)).to eq false
    end

    it 'returns true if the value is found' do
      expect(described_class.new(14).compare_and_set(14, 2)).to eq true
    end

    it 'sets if the value is found' do
      f = described_class.new(14)
      f.compare_and_set(14, 2)
      expect(f.value).to eq 2
    end

    it 'does not set if the value is not found' do
      f = described_class.new(14)
      f.compare_and_set(2, 12)
      expect(f.value).to eq 14
    end
  end

  context '#update' do

    it 'passes the current value to the block' do
      atomic = described_class.new(1000)
      atomic.update { |v| (expect(v).to eq 1000); 1 }
    end

    it 'atomically sets the value to the return value from the block' do
      atomic = described_class.new(1000)
      atomic.update { |v| v + 1 }
      expect(atomic.value).to eq 1001
    end

    it 'returns the new value' do
      atomic = described_class.new(1000)
      expect(atomic.update { |v| v + 1 }).to eq 1001
    end
  end
end

module Concurrent

  RSpec.describe MutexAtomicFixnum do

    it_should_behave_like :atomic_fixnum

    context 'construction' do

      it 'raises an exception if the initial value is too big' do
        expect {
          described_class.new(Utility::NativeInteger::MAX_VALUE + 1)
        }.to raise_error(RangeError)
      end

      it 'raises an exception if the initial value is too small' do
        expect {
          described_class.new(Utility::NativeInteger::MIN_VALUE - 1)
        }.to raise_error(RangeError)
      end
    end

    context 'instance methods' do

      before(:each) do
        expect(subject).to receive(:synchronize).with(no_args).and_call_original
      end

      specify 'value is synchronized' do
        subject.value
      end

      specify 'value= is synchronized' do
        subject.value = 10
      end

      specify 'increment is synchronized' do
        subject.increment
      end

      specify 'decrement is synchronized' do
        subject.decrement
      end

      specify 'compare_and_set is synchronized' do
        subject.compare_and_set(14, 2)
      end
    end
  end

  if Concurrent.allow_c_extensions?
    RSpec.describe CAtomicFixnum do
      it_should_behave_like :atomic_fixnum
    end
  end

  if Concurrent.on_jruby?
    RSpec.describe JavaAtomicFixnum do
      it_should_behave_like :atomic_fixnum
    end
  end

  RSpec.describe AtomicFixnum do
    if RUBY_ENGINE != 'ruby'
      it 'does not load the C extension' do
        expect(defined?(Concurrent::CAtomicFixnum)).to be_falsey
      end
    end

    if Concurrent.on_jruby?
      it 'inherits from JavaAtomicFixnum' do
        expect(AtomicFixnum.ancestors).to include(JavaAtomicFixnum)
      end
    elsif Concurrent.allow_c_extensions?
      it 'inherits from CAtomicFixnum' do
        expect(AtomicFixnum.ancestors).to include(CAtomicFixnum)
      end
    else
      it 'inherits from MutexAtomicFixnum' do
        expect(AtomicFixnum.ancestors).to include(MutexAtomicFixnum)
      end
    end

    describe '#to_s and #inspect' do
      it 'includes the value' do
        subject = described_class.new(42)
        expect(subject.to_s).to include('42')
        expect(subject.inspect).to include('42')
      end
    end

  end
end
