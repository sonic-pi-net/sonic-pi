#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Channel Direction
# https://gobyexample.com/channel-directions

# we can't force a channel to go only one direction w/i a function
# but we can replicate the actual functionality from the example

def ping(pings, msg)
  pings << msg
end

def pong(pings, pongs)
  msg = ~pings
  pongs << msg
end

pings = Channel.new(capacity: 1) # buffered
pongs = Channel.new(capacity: 1) # buffered

ping(pings, 'passed message')
pong(pings, pongs)

puts ~pongs

__END__
passed message
