#!/usr/bin/env ruby

#$: << File.expand_path('../../lib', __FILE__)

$DEBUG_TLV = true
require 'concurrent'
require 'concurrent/atomic/thread_local_var'
require 'benchmark'
require 'thread'

include Concurrent

# if we hold on to vars, but threads die, space used for TLVs should be recovered

def test_thread_gc(vars)
  threads = 500.times.collect do
    Thread.new do
      vars.each do |var|
        var.value = 1
      end
    end
  end
  threads.each(&:join)
end

puts "BEFORE THREAD GC TEST:"
puts "Ruby heap pages: #{GC.stat[:heap_length]}, Other malloc'd bytes: #{GC.stat[:malloc_increase]}"

vars = 500.times.collect { ThreadLocalVar.new(0) }
200.times do
  test_thread_gc(vars)
  GC.start
end

puts "AFTER THREAD GC TEST:"
puts "Ruby heap pages: #{GC.stat[:heap_length]}, Other malloc'd bytes: #{GC.stat[:malloc_increase]}"

# if we hold on to threads, but drop TLVs, space used should be reused by allocated TLVs

def tlv_gc_test_loop(queue)
  while true
    var = queue.pop
    return if var.nil?
    var.value = 1
  end
end

def test_tlv_gc(queues)
  500.times do
    var = ThreadLocalVar.new(0)
    queues.each { |q| q << var }
  end
end

puts
puts "BEFORE TLV GC TEST:"
puts "Ruby heap pages: #{GC.stat[:heap_length]}, Other malloc'd bytes: #{GC.stat[:malloc_increase]}"

queues  = 500.times.collect { Queue.new }
threads = queues.map do |queue|
  Thread.new do
    tlv_gc_test_loop(queue)
  end
end

200.times do
  test_tlv_gc(queues)
  GC.start
end
queues.each { |q| q << nil }
threads.each(&:join)

puts "AFTER TLV GC TEST:"
puts "Ruby heap pages: #{GC.stat[:heap_length]}, Other malloc'd bytes: #{GC.stat[:malloc_increase]}"
