#!/usr/bin/env ruby

$: << File.expand_path('../../lib', __FILE__)

require 'optparse'
require 'thread'
require 'benchmark'

require 'concurrent/atomics'

unless defined? Concurrent::CAtomicReference
  warn "[ERROR] C extensions not loaded!"
  exit(1)
end

Thread.abort_on_exception = true

$conf = {
  :lock => "atomic",
  :num_threads => 100,
  :count => 100_000,
  :count_per_thread => nil,
  :slow => nil,
}

OptionParser.new do |opts|
  opts.on("-c", "--count NUM") do |n|
    $conf[:count] = n.to_i
  end
  opts.on("-p", "--count-per-thread") do |n|
    $conf[:count_per_thread] = n.to_i
  end
  opts.on("-t", "--num-threads NUM") do |n|
    $conf[:num_threads] = n.to_i
  end
  opts.on("-s", "--slow NUM") do |n|
    $conf[:slow] = n.to_i
  end
  opts.on("-l", "--lock atomic|mutex") do |x|
    $conf[:lock] = x
  end
  opts.on("-h", "--help"){ puts opts; exit }
end.parse!(ARGV)

unless $conf[:count_per_thread]
  $conf[:count_per_thread] = $conf[:count] / $conf[:num_threads]
end
$conf.delete(:count)

if $conf[:slow].to_i > 0
  require 'digest/md5'
  def slow_down
    $conf[:slow].times do |i|
      Digest::MD5.hexdigest(i.to_s)
    end
  end

  ret = []
  10.times do
    m = Benchmark.measure{ slow_down }
    ret << m.real
  end

  $conf[:slow_time] = [ret.min, ret.max]
else
  def slow_down; end
end

$stderr.puts $conf.inspect

def para_prepare(&block)
  num_threads = $conf[:num_threads]
  count = $conf[:count_per_thread]

  if num_threads % 2 > 0
    raise ArgumentError, "num_threads must be a multiple of two"
  end

  # Keep those threads together
  tg = ThreadGroup.new

  num_threads.times do |i|
    diff = (i % 2 == 0) ? 1 : -1

    t = Thread.new do
      nil until $go
      count.times do
        yield diff
      end
    end

    tg.add(t)
  end

  # Make sure all threads are started
  while tg.list.find{|t| t.status != "run"}
    Thread.pass
  end

  # For good measure
  GC.start

  $go = false

  tg
end



$tg = nil
if $conf[:lock] == "atomic"
  $atom = Concurrent::Atomic.new(0)
  $tg = para_prepare do |diff|
    $atom.update do |x|
      slow_down
      x + diff
    end
  end
else
  $lock = Mutex.new
  $value = 0
  $tg = para_prepare do |diff|
    $lock.synchronize do
      slow_down
      $value += diff
    end
  end
end


# Run !
#
# NOTE: It seems to me that this measurement method
#       is sensible to how the system dispatches his resources.
#
#       More precise caluclation could be done using
#       getrusage's times
ret = Benchmark.measure do
  $go = true
  $tg.list.each{|t| t.join}
  $go = false
end
puts ret.real
