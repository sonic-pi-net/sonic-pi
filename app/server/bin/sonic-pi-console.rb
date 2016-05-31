#!/usr/bin/env ruby
require 'socket'

host = 'localhost'
port = 4558

puts 'Starting sonic pi cli console'

s = UDPSocket.new
s.bind(nil, port)

loop do
  message, sender = s.recvfrom(1000000)
  puts message.split(/[^[:print:]]/).reject { |c| c.empty? }.to_s
end
