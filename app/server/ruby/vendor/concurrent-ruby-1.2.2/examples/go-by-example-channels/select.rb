#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Select
# https://gobyexample.com/select

c1 = Channel.new # unbuffered
c2 = Channel.new # unbuffered

Channel.go do
  sleep(1)
  c1 << 'one'
end

Channel.go do
  sleep(2)
  c1 << 'two'
end

2.times do
  Channel.select do |s|
    s.take(c1) { |msg| print "received #{msg}\n" }
    s.take(c2) { |msg| print "received #{msg}\n" }
  end
end

__END__
received one
received two
