#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++


# Uncomment this file if you want to run benchmark tests
# requires benchmark/ips gem to be installed


# require_relative '../setup_test.rb'
# require_relative "../../lib/sonicpi/oscdecode"
# require 'benchmark/ips'
# require 'osc-ruby'

# samosc = SonicPi::OscDecode.new
# oscruby = OSC::OSCPacket

# test_message = OSC::Message.new("/foo", ["beans", 1, 2.0]).encode

# Benchmark.ips do |bencher|
#   bencher.report("samsosc") { samosc.decode_single_message(test_message) }
#   bencher.report("oscruby") { oscruby.messages_from_network(test_message) }

#   bencher.compare
# end
