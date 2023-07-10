#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## Go by Example: Tickers
# https://gobyexample.com/tickers

ticker = Channel.ticker(0.5)
Channel.go do
  ticker.each do |tick|
    print "Tick at #{tick}\n" if tick
  end
end

sleep(1.6)
ticker.stop
print "Ticker stopped\n"

__END__
Tick at 2012-09-23 11:29:56.487625 -0700 PDT
Tick at 2012-09-23 11:29:56.988063 -0700 PDT
Tick at 2012-09-23 11:29:57.488076 -0700 PDT
Ticker stopped
