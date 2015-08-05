require 'benchmark'
require_relative 'example_group_extensions'
require_relative 'platform_helpers'

require 'concurrent/atomics'

def atomic_reference_test(clazz, opts = {})
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

describe Concurrent::AtomicReference do

  let!(:threads) { 10 }
  let!(:tests) { 1000 }

  describe Concurrent::MutexAtomicReference do

    specify 'is defined' do
      expect(defined?(Concurrent::MutexAtomicReference)).to be_truthy
    end

    specify 'runs the benchmarks' do
      stats = atomic_reference_test('MutexAtomicReference', threads: threads, tests: tests)
      expect(stats).to be_benchmark_results
    end
  end

  if jruby? && 'JRUBY' == ENV['TEST_PLATFORM'].strip

    describe Concurrent::JavaAtomicReference do

      specify 'Concurrent::JavaAtomicReference is defined' do
        expect(defined?(Concurrent::JavaAtomicReference)).to be_truthy
      end

      specify 'runs the benchmarks' do
        stats = atomic_reference_test('JavaAtomicReference', threads: threads, tests: tests)
        expect(stats).to be_benchmark_results
      end
    end

  else

    specify 'Concurrent::JavaAtomicReference is not defined' do
      expect(defined?(Concurrent::JavaAtomicReference)).to be_falsey
    end
  end

  if 'EXT' == ENV['TEST_PLATFORM'].strip

    describe Concurrent::CAtomicReference do

      specify 'Concurrent::CAtomicReference is defined' do
        expect(defined?(Concurrent::CAtomicReference)).to be_truthy
      end

      specify 'runs the benchmarks' do
        stats = atomic_reference_test('CAtomicReference', threads: threads, tests: tests)
        expect(stats).to be_benchmark_results
      end
    end

  else

    specify 'Concurrent::CAtomicReference is not defined' do
      expect(defined?(Concurrent::CAtomicReference)).to be_falsey
    end
  end
end
