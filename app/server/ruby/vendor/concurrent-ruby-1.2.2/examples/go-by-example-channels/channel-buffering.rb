#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Channel Buffering
# https://gobyexample.com/channel-buffering

messages = Channel.new(capacity: 2) # buffered

messages.put 'buffered'
messages.put 'channel'

puts messages.take
puts messages.take

__END__
buffered
channel
