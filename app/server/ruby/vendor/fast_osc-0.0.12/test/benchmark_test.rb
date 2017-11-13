#--
# This file was part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++


# requires benchmark/ips gem to be installed

require 'test_helper'
require 'benchmark/ips'
require 'osc-ruby'
require 'fast_osc/pure_ruby_fallback_encode'
require 'fast_osc/pure_ruby_fallback_decode'

samosc = SonicPi::OSC::OscDecode.new(true)
samoscenc = SonicPi::OSC::OscEncode.new(false)
oscruby = OSC::OSCPacket

address = "/feeooblah"
args = ["beans", 1, 2.0]
msg = OSC::Message.new(address, *args)
test_message = msg.encode
# puts test_message.inspect

# puts [address, args].inspect
puts "ENCODING TEST"
Benchmark.ips do |bencher|
  bencher.report("fast_osc") { FastOsc.encode_single_message(address, args) }
  bencher.report("samsosc") { samoscenc.encode_single_message(address, args) }
  bencher.report("osc-ruby") { msg.encode }

  bencher.compare
end

# puts samosc.decode_single_message(test_message).inspect
puts "DECODING TEST"
Benchmark.ips do |bencher|
  bencher.report("fast_osc") { FastOsc.decode_single_message(test_message) }
  bencher.report("samsosc") { samosc.decode_single_message(test_message) }
  bencher.report("osc-ruby") { oscruby.messages_from_network(test_message) }

  bencher.compare
end
