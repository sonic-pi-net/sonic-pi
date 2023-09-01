#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## A Tour of Go: Range and Close
# https://tour.golang.org/concurrency/4

def fibonacci(n, c)
  x, y = 0, 1
  (1..n).each do
    c << x
    x, y = y, x+y
  end
  c.close
end

c = Channel.new(capacity: 10)
Channel.go { fibonacci(c.capacity, c) }
c.each { |i| puts i }

__END__
0
1
1
2
3
5
8
13
21
34
