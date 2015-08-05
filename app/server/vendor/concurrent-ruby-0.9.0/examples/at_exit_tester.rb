#!/usr/bin/env ruby

$: << File.expand_path('../../lib', __FILE__)

require 'concurrent'

# with this, a JRuby process will never exit
Concurrent.disable_executor_auto_termination!

SIZE = 5

pool = Concurrent::FixedThreadPool.new(SIZE)

latch = Concurrent::CountDownLatch.new(SIZE)

print "Posting #{SIZE} tasks...\n"

SIZE.times do |i|
  pool.post do
    this = i
    sleep(1)
    print "Completing task #{i}\n"
    latch.count_down
  end
end

print "Waiting for all tasks to complete...\n"
latch.wait
print "Done\n"
