#!/usr/bin/env ruby

$: << File.expand_path('../../lib', __FILE__)

require 'benchmark'
require 'concurrent/executors'

COUNT = 100_000

executor = Concurrent::CachedThreadPool.new
latch = Concurrent::CountDownLatch.new

COUNT.times { executor.post{ nil } }

#COUNT.times do |i|
#  executor.post{ nil }
#  sleep(0.01) if i % 1000 == 0
#end

executor.post{ latch.count_down }
latch.wait

puts "Max length:           #{executor.max_length}" if executor.respond_to?(:max_length)
puts "Largest length:       #{executor.largest_length}" if executor.respond_to?(:largest_length)
puts "Scheduled task count: #{executor.scheduled_task_count}" if executor.respond_to?(:scheduled_task_count)
puts "Completed task count: #{executor.completed_task_count}" if executor.respond_to?(:completed_task_count)
