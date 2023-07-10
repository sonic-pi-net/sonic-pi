#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## A Tour of Go: Default Selection
# https://tour.golang.org/concurrency/6

tick = Channel.tick(0.1)
boom = Channel.after(0.5)

loop do
  Channel.select do |s|
    s.take(tick) { |t| print "tick.\n" if t }
    s.take(boom) do
      print "BOOM!\n"
      exit
    end
    s.default do
      print "    .\n"
      sleep(0.05)
    end
  end
end

__END__
    .
    .
tick.
    .
    .
tick.
    .
    .
tick.
    .
    .
tick.
    .
    .
tick.
BOOM!
