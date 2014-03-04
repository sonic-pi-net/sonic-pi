#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "core.rb"
require 'edn'
incoming = OSC::Server.new(4558)

incoming.add_method("/reply") do |payload|
  info = EDN.read(payload.to_a[0])
  case info[:type]
  when :message
    puts info[:val]
  when :error
    STDERR.puts info[:val]
  else
    #puts info[:type]
  end

  STDOUT.flush
end

puts "starting up..."
t1 = Thread.new{incoming.run}

t1.join
