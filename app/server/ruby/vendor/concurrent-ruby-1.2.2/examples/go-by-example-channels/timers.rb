#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Timers
# https://gobyexample.com/timers

timer1 = Channel.timer(2)

puts 'Timer 1 expired' if ~timer1

timer2 = Channel.timer(1)
Channel.go do
  print "Timer 2 expired\n" if ~timer2
end

stop2 = timer2.stop
print "Timer 2 stopped\n" if stop2

__END__
Timer 1 expired
Timer 2 stopped
