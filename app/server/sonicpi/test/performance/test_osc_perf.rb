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

if ENV['RUN_PERF_TESTS']
  require_relative '../setup_test.rb'
  require_relative "../../lib/sonicpi/oscdecode"
  require_relative "../../lib/sonicpi/oscencode"
  require 'benchmark/ips'
  require 'osc-ruby'

  samosc = SonicPi::OscDecode.new(false)
  samoscenc = SonicPi::OscEncode.new(false)
  oscruby = OSC::OSCPacket

  address = "/feeooblah"
  args = ["beans", 1, 2.0] 
  msg = OSC::Message.new(address, *args)
  test_message = msg.encode

  Benchmark.ips do |bencher|
    bencher.report("samsosc") { samoscenc.encode_single_message(address, args) }
    bencher.report("oscruby") { msg.encode }

    bencher.compare
  end

  # Benchmark.ips do |bencher|
  #   bencher.report("samsosc") { samosc.decode_single_message(test_message) }
  #   bencher.report("oscruby") { oscruby.messages_from_network(test_message) }
  # 
  #   bencher.compare
  # end
end
