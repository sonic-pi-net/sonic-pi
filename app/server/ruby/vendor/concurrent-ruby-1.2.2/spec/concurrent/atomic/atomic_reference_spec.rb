require 'concurrent/atomic/atomic_reference'

RSpec.shared_examples :atomic_reference do

  specify :test_construct do
    atomic = described_class.new
    expect(atomic.value).to be_nil

    atomic = described_class.new(0)
    expect(atomic.value).to eq 0
  end

  specify :test_value do
    atomic       = described_class.new(0)
    atomic.value = 1

    expect(atomic.value).to eq 1
  end

  specify :test_update do
    # use a number outside JRuby's fixnum cache range, to ensure identity is preserved
    atomic = described_class.new(1000)
    res    = atomic.update { |v| v + 1 }

    expect(atomic.value).to eq 1001
    expect(res).to eq 1001
  end

  specify :test_try_update do
    # use a number outside JRuby's fixnum cache range, to ensure identity is preserved
    atomic = described_class.new(1000)
    res    = atomic.try_update { |v| v + 1 }

    expect(atomic.value).to eq 1001
    expect(res).to eq 1001
  end

  specify :test_try_update_bang do
    # use a number outside JRuby's fixnum cache range, to ensure identity is preserved
    atomic = described_class.new(1000)
    res    = atomic.try_update! { |v| v + 1 }

    expect(atomic.value).to eq 1001
    expect(res).to eq 1001
  end

  specify :test_swap do
    atomic = described_class.new(1000)
    res    = atomic.swap(1001)

    expect(atomic.value).to eq 1001
    expect(res).to eq 1000
  end

  specify :test_try_update_fails do
    # use a number outside JRuby's fixnum cache range, to ensure identity is preserved
    atomic = described_class.new(1000)
    expect(
        # assigning within block exploits implementation detail for test
        atomic.try_update { |v| atomic.value = 1001; v + 1 }
    ).to be_falsey
  end

  specify :test_try_update_bang_fails do
    # use a number outside JRuby's fixnum cache range, to ensure identity is preserved
    atomic = described_class.new(1000)
    expect {
      # assigning within block exploits implementation detail for test
      atomic.try_update! { |v| atomic.value = 1001; v + 1 }
    }.to raise_error Concurrent::ConcurrentUpdateError
  end

  specify :test_update_retries do
    tries = 0
    # use a number outside JRuby's fixnum cache range, to ensure identity is preserved
    atomic = described_class.new(1000)
    # assigning within block exploits implementation detail for test
    atomic.update { |v| tries += 1; atomic.value = 1001; v + 1 }

    expect(tries).to eq 2
  end

  specify :test_numeric_cas do
    atomic = described_class.new(0)

    # 9-bit idempotent Fixnum (JRuby)
    max_8 = 2 ** 256 - 1
    min_8 = -(2 ** 256)

    atomic.set(max_8)
    max_8.upto(max_8 + 2) do |i|
      expect(atomic.compare_and_swap(i, i + 1)).to be_truthy, "CAS failed for numeric #{i} => #{i + 1}"
    end

    atomic.set(min_8)
    min_8.downto(min_8 - 2) do |i|
      expect(atomic.compare_and_swap(i, i - 1)).to be_truthy, "CAS failed for numeric #{i} => #{i - 1}"
    end

    # 64-bit idempotent Fixnum (MRI, TruffleRuby)
    max_64 = 2 ** 62 - 1
    min_64 = -(2 ** 62)

    atomic.set(max_64)
    max_64.upto(max_64 + 2) do |i|
      expect(atomic.compare_and_swap(i, i + 1)).to be_truthy, "CAS failed for numeric #{i} => #{i + 1}"
    end

    atomic.set(min_64)
    min_64.downto(min_64 - 2) do |i|
      expect(atomic.compare_and_swap(i, i - 1)).to be_truthy, "CAS failed for numeric #{i} => #{i - 1}"
    end

    ## 64-bit overflow into Bignum (JRuby)
    max_64 = 2 ** 63 - 1
    min_64 = (-2 ** 63)

    atomic.set(max_64)
    max_64.upto(max_64 + 2) do |i|
      expect(atomic.compare_and_swap(i, i + 1)).to be_truthy, "CAS failed for numeric #{i} => #{i + 1}"
    end

    atomic.set(min_64)
    min_64.downto(min_64 - 2) do |i|
      expect(atomic.compare_and_swap(i, i - 1)).to be_truthy, "CAS failed for numeric #{i} => #{i - 1}"
    end

    # non-idempotent Float (JRuby, MRI < 2.0.0 or 32-bit)
    atomic.set(1.0 + 0.1)
    expect(atomic.compare_and_set(1.0 + 0.1, 1.2)).to be_truthy, "CAS failed for #{1.0 + 0.1} => 1.2"

    # Bignum
    atomic.set(2 ** 100)
    expect(atomic.compare_and_set(2 ** 100, 0)).to be_truthy, "CAS failed for #{2 ** 100} => 0"

    # Rational
    require 'rational' unless ''.respond_to? :to_r
    atomic.set(Rational(1, 3))
    expect(atomic.compare_and_set(Rational(1, 3), 0)).to be_truthy, "CAS failed for #{Rational(1, 3)} => 0"

    # Complex
    require 'complex' unless ''.respond_to? :to_c
    atomic.set(Complex(1, 2))
    expect(atomic.compare_and_set(Complex(1, 2), 0)).to be_truthy, "CAS failed for #{Complex(1, 2)} => 0"
  end
end

module Concurrent

  RSpec.describe AtomicReference do
    it_should_behave_like :atomic_reference

    describe '#to_s and #inspect' do
      it 'includes the value' do
        subject = described_class.new('kajhsd')
        expect(subject.to_s).to include('kajhsd')
        expect(subject.inspect).to include('kajhsd')
      end
    end
  end

  RSpec.describe MutexAtomicReference do
    it_should_behave_like :atomic_reference
  end

  if Concurrent.allow_c_extensions?
    RSpec.describe CAtomicReference do
      it_should_behave_like :atomic_reference
    end
  end
  if Concurrent.on_jruby?
    RSpec.describe JavaAtomicReference do
      it_should_behave_like :atomic_reference
    end
  end
  if Concurrent.on_truffleruby?
    RSpec.describe TruffleRubyAtomicReference do
      it_should_behave_like :atomic_reference
    end
  end

  RSpec.describe AtomicReference do
    if RUBY_ENGINE != 'ruby'
      it 'does not load the C extension' do
        expect(defined?(Concurrent::CAtomicReference)).to be_falsey
      end
    end

    if Concurrent.on_jruby?
      it 'inherits from JavaAtomicReference' do
        expect(described_class.ancestors).to include(Concurrent::JavaAtomicReference)
      end
    elsif Concurrent.allow_c_extensions?
      it 'inherits from CAtomicReference' do
        expect(described_class.ancestors).to include(Concurrent::CAtomicReference)
      end
    elsif Concurrent.on_truffleruby?
      it 'inherits from TruffleRubyAtomicReference' do
        expect(described_class.ancestors).to include(Concurrent::TruffleRubyAtomicReference)
      end
    else
      it 'inherits from MutexAtomicReference' do
        expect(described_class.ancestors).to include(Concurrent::MutexAtomicReference)
      end
    end
  end
end
