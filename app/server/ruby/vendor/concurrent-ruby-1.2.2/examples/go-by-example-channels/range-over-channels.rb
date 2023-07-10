#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Range over Channels
# https://gobyexample.com/range-over-channels

queue = Channel.new(capacity: 2) # buffered
queue << 'one'
queue << 'two'
queue.close

queue.each do |elem|
  print "#{elem}\n"
end

__END__
one
two
