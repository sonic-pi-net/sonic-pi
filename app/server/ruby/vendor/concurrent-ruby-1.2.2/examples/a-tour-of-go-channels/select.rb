#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## A Tour of Go: Select
# https://tour.golang.org/concurrency/5

def fibonacci(c, quit)
  x, y = 0, 1
  loop do
    Channel.select do |s|
      s.case(c, :<<, x) { x, y = y, x+y; x } # alias for `s.put`
      s.case(quit, :~) do                    # alias for `s.take`
        puts 'quit'
        return
      end
    end
  end
end

c = Channel.new
quit = Channel.new

Channel.go do
  10.times { puts ~c }
  quit << 0
end

fibonacci(c, quit)

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
quit
