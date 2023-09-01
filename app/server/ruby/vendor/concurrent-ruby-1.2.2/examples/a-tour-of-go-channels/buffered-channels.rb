#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## A Tour of Go: Buffered Channels
# https://tour.golang.org/concurrency/3

ch = Channel.new(capacity: 2)
ch << 1
ch << 2

puts ~ch
puts ~ch

__END__
1
2
