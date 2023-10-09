#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Unbuffered Channel
# https://gobyexample.com/channels

messages = Channel.new # unbuffered

Channel.go do
  messages.put 'ping'
end

msg = messages.take
puts msg

__END__
ping
