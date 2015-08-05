#!/usr/bin/env ruby

$: << File.expand_path('../../lib', __FILE__)

require 'benchmark'
require 'thread'

class MonotonicClock
  def initialize
    @mutex = Mutex.new
    @last_time = Time.now.to_f
  end

  def get_time_native
    Process.clock_gettime(Process::CLOCK_MONOTONIC)
  end

  def get_interval_native(since)
    Process.clock_gettime(Process::CLOCK_MONOTONIC) - since.to_f
  end

  def get_time_ruby
    @mutex.synchronize do
      now = Time.now.to_f
      if @last_time < now
        @last_time = now 
      else # clock has moved back in time
        @last_time +=  0.000_001
      end
    end
  end

  def get_interval_ruby(since)
    get_time_ruby - since.to_f
  end
end

COUNT = 2_000_000
CLOCK = MonotonicClock.new

native_now = CLOCK.get_time_native
ruby_now = CLOCK.get_time_ruby

puts "Native: #{native_now}, Ruby: #{ruby_now}"

Benchmark.bmbm do |bm|

  bm.report('Native time') do
    COUNT.times{ CLOCK.get_time_native }
  end

  bm.report('Ruby time') do
    COUNT.times{ CLOCK.get_time_ruby }
  end

  bm.report('Native interval') do
    COUNT.times{ CLOCK.get_interval_native(native_now) }
  end

  bm.report('Ruby interval') do
    COUNT.times{ CLOCK.get_interval_ruby(ruby_now) }
  end
end

puts "Native: #{CLOCK.get_time_native}, Ruby: #{CLOCK.get_time_ruby}"
