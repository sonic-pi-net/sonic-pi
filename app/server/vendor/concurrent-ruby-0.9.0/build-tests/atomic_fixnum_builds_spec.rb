require 'benchmark'
require_relative 'example_group_extensions'
require_relative 'platform_helpers'

require 'concurrent/atomics'

def atomic_fixnum_test(clazz, opts = {})
  threads = opts.fetch(:threads, 5)
  tests = opts.fetch(:tests, 100)

  atomic = Concurrent.const_get(clazz.to_s).new
  latch = Concurrent::CountDownLatch.new(threads)

  stats = Benchmark.measure do
    threads.times do |i|
      Thread.new do
        tests.times{ atomic.up }
        latch.count_down
      end
    end
    latch.wait
  end
  stats
end

describe Concurrent::AtomicFixnum do

  let!(:threads) { 10 }
  let!(:tests) { 1000 }

  describe Concurrent::MutexAtomicFixnum do

    specify 'is defined' do
      expect(defined?(Concurrent::MutexAtomicFixnum)).to be_truthy
    end

    specify 'runs the benchmarks' do
      stats = atomic_fixnum_test('MutexAtomicFixnum', threads: threads, tests: tests)
      expect(stats).to be_benchmark_results
    end
  end

  if jruby?

    describe Concurrent::JavaAtomicFixnum do

      specify 'Concurrent::JavaAtomicFixnum is defined' do
        expect(defined?(Concurrent::JavaAtomicFixnum)).to be_truthy
      end

      specify 'runs the benchmarks' do
        stats = atomic_fixnum_test('JavaAtomicFixnum', threads: threads, tests: tests)
        expect(stats).to be_benchmark_results
      end
    end

  else

    specify 'Concurrent::JavaAtomicFixnum is not defined' do
      expect(defined?(Concurrent::JavaAtomicFixnum)).to be_falsey
    end
  end

  if 'EXT' == ENV['TEST_PLATFORM'].strip

    describe Concurrent::CAtomicFixnum do

      specify 'Concurrent::CAtomicFixnum is defined' do
        expect(defined?(Concurrent::CAtomicFixnum)).to be_truthy
      end

      specify 'runs the benchmarks' do
        stats = atomic_fixnum_test('CAtomicFixnum', threads: threads, tests: tests)
        expect(stats).to be_benchmark_results
      end
    end

  else

    specify 'Concurrent::CAtomicFixnum is not defined' do
      expect(defined?(Concurrent::CAtomicFixnum)).to be_falsey
    end
  end
end
