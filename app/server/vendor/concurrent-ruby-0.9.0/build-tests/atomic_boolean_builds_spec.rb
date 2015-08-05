require 'benchmark'
require_relative 'example_group_extensions'
require_relative 'platform_helpers'

require 'concurrent/atomics'

def atomic_boolean_test(clazz, opts = {})
  threads = opts.fetch(:threads, 5)
  tests = opts.fetch(:tests, 100)

  atomic = Concurrent.const_get(clazz.to_s).new
  latch = Concurrent::CountDownLatch.new(threads)

  stats = Benchmark.measure do
    threads.times do |i|
      Thread.new do
        tests.times{ atomic.value = true }
        latch.count_down
      end
    end
    latch.wait
  end
  stats
end

describe Concurrent::AtomicBoolean do

  let!(:threads) { 10 }
  let!(:tests) { 1000 }

  describe Concurrent::MutexAtomicBoolean do

    specify 'is defined' do
      expect(defined?(Concurrent::MutexAtomicBoolean)).to be_truthy
    end

    specify 'runs the benchmarks' do
      stats = atomic_boolean_test('MutexAtomicBoolean', threads: threads, tests: tests)
      expect(stats).to be_benchmark_results
    end
  end

  if jruby?

    describe Concurrent::JavaAtomicBoolean do

      specify 'Concurrent::JavaAtomicBoolean is defined' do
        expect(defined?(Concurrent::JavaAtomicBoolean)).to be_truthy
      end

      specify 'runs the benchmarks' do
        stats = atomic_boolean_test('JavaAtomicBoolean', threads: threads, tests: tests)
        expect(stats).to be_benchmark_results
      end
    end

  else

    specify 'Concurrent::JavaAtomicBoolean is not defined' do
      expect(defined?(Concurrent::JavaAtomicBoolean)).to be_falsey
    end
  end

  if 'EXT' == ENV['TEST_PLATFORM'].strip

    describe Concurrent::CAtomicBoolean do

      specify 'Concurrent::CAtomicBoolean is defined' do
        expect(defined?(Concurrent::CAtomicBoolean)).to be_truthy
      end

      specify 'runs the benchmarks' do
        stats = atomic_boolean_test('CAtomicBoolean', threads: threads, tests: tests)
        expect(stats).to be_benchmark_results
      end
    end

  else

    specify 'Concurrent::CAtomicBoolean is not defined' do
      expect(defined?(Concurrent::CAtomicBoolean)).to be_falsey
    end
  end
end
