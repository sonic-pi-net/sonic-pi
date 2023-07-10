#!/usr/bin/env ruby

#$: << File.expand_path('../../lib', __FILE__)

require 'concurrent/atomics'

my_atomic = Concurrent::AtomicReference.new(0)
my_atomic.update {|v| v + 1}
puts "new value: #{my_atomic.value}"

begin
  my_atomic.try_update {|v| v + 1}
rescue Concurrent::Atomic::ConcurrentUpdateError => cue
  # deal with it (retry, propagate, etc)
end
puts "new value: #{my_atomic.value}"
