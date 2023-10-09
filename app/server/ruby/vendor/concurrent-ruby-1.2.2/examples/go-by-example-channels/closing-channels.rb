#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Closing Channels
# https://gobyexample.com/closing-channels

validator = ->(v){ v.is_a? Numeric }
jobs = Channel.new(buffer: :buffered, capacity: 5,
                   validator: validator)
done = Channel.new(buffer: :unbuffered)

Channel.go_loop do
  j, more = jobs.next
  if more
    print "received job #{j}\n"
    true # loop again
  else
    print "received all jobs\n"
    done << true
    false # exit the loop
  end
end

(1..3).each do |i|
  jobs << i
  print "sent job #{i}\n"
  Thread.pass # give the worker a chance to run
end

jobs.close
print "sent all jobs\n"
~done

__END__
sent job 1
received job 1
sent job 2
received job 2
sent job 3
received job 3
sent all jobs
received all jobs
