#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## A Tour of Go: Channels
# https://tour.golang.org/concurrency/2

def sum(a, c)
  sum = a.reduce(0, &:+)
  c << sum # `<<` is an alias for `put` or `send`
end

a = [7, 2, 8, -9, 4, 0]
l = a.length / 2
c = Channel.new

Channel.go { sum(a[-l, l], c) }
Channel.go { sum(a[0, l], c) }
x, y = ~c, ~c # `~` is an alias for `take` or `receive`

puts [x, y, x+y].join(' ')

__END__
-5 17 12
