#!/usr/bin/env ruby

#$: << File.expand_path('../../lib', __FILE__)

require 'concurrent/atomic/read_write_lock'
require 'benchmark'
require 'optparse'
require 'ostruct'

$options = OpenStruct.new
$options.threads = 100
$options.interleave = false
$options.compare = false

OptionParser.new do |opts|
  opts.banner = "Usage: #{File.basename(__FILE__)} [options]"

  opts.on('-t', '--threads=THREADS', OptionParser::DecimalInteger, "Number of threads per test (default #{$options.threads})") do |value|
    $options.threads = value
  end

  opts.on('-i', '--[no-]interleave', 'Interleave output to check for starvation') do |value|
    $options.interleave = value
  end

  opts.on('-c', '--[no-]compare', 'Compare with other implementations') do |value|
    $options.compare = value
  end

  opts.on('-h', '--help', 'Prints this help') do
    puts opts
    exit
  end
end.parse!

def jruby?
  RUBY_ENGINE == "jruby"
end

# for performance comparison with ReadWriteLock
class SimpleMutex
  def initialize; @mutex = Mutex.new; end
  def with_read_lock
    @mutex.synchronize { yield }
  end
  alias :with_write_lock :with_read_lock
end

# for seeing whether my correctness test is doing anything...
# and for seeing how great the overhead of the test is
# (apart from the cost of locking)
class FreeAndEasy
  def with_read_lock
    yield # thread safety is for the birds... I prefer to live dangerously
  end
  alias :with_write_lock :with_read_lock
end

if jruby?
  # the Java platform comes with a read-write lock implementation
  # performance is very close to ReadWriteLock, but just a *bit* slower
  require 'java'
  class JavaReadWriteLock
    def initialize
      @lock = java.util.concurrent.locks.ReentrantReadWriteLock.new
    end
    def with_read_lock
      @lock.read_lock.lock
      result = yield
      @lock.read_lock.unlock
      result
    end
    def with_write_lock
      @lock.write_lock.lock
      result = yield
      @lock.write_lock.unlock
      result
    end
  end
end

def test(lock)
  puts "READ INTENSIVE (80% read, 20% write):"
  single_test(lock, ($options.threads * 0.8).floor, ($options.threads * 0.2).floor)
  puts "WRITE INTENSIVE (80% write, 20% read):"
  single_test(lock, ($options.threads * 0.2).floor, ($options.threads * 0.8).floor)
  puts "BALANCED (50% read, 50% write):"
  single_test(lock, ($options.threads * 0.5).floor, ($options.threads * 0.5).floor)
end

def single_test(lock, n_readers, n_writers, reader_iterations=50, writer_iterations=50, reader_sleep=0.001, writer_sleep=0.001)
  puts "Testing #{lock.class} with #{n_readers} readers and #{n_writers} writers. Readers iterate #{reader_iterations} times, sleeping #{reader_sleep}s each time, writers iterate #{writer_iterations} times, sleeping #{writer_sleep}s each time"
  mutex = Mutex.new
  bad   = false
  data  = 0

  result = Benchmark.measure do
    readers = n_readers.times.collect do
      Thread.new do
        reader_iterations.times do
          lock.with_read_lock do
            print "r" if $options.interleave
            mutex.synchronize { bad = true } if (data % 2) != 0
            sleep(reader_sleep)
            mutex.synchronize { bad = true } if (data % 2) != 0
          end
        end
      end
    end
    writers = n_writers.times.collect do
      Thread.new do
        writer_iterations.times do
          lock.with_write_lock do
            print "w" if $options.interleave
            # invariant: other threads should NEVER see "data" as an odd number
            value = (data += 1)
            # if a reader runs right now, this invariant will be violated
            sleep(writer_sleep)
            # this looks like a strange way to increment twice;
            # it's designed so that if 2 writers run at the same time, at least
            #   one increment will be lost, and we can detect that at the end
            data  = value+1 
          end
        end
      end
    end

    readers.each { |t| t.join }
    writers.each { |t| t.join }
    puts "BAD!!! Readers+writers overlapped!" if mutex.synchronize { bad }
    puts "BAD!!! Writers overlapped!" if data != (n_writers * writer_iterations * 2)
  end
  puts result
end

test(Concurrent::ReadWriteLock.new)
test(JavaReadWriteLock.new) if $options.compare && jruby?
test(SimpleMutex.new) if $options.compare
test(FreeAndEasy.new) if $options.compare
