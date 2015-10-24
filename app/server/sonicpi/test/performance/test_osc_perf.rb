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
  require_relative "../../lib/sonicpi/osc/osc"
  require 'benchmark/ips'

  decoder = SonicPi::OSC::OscDecode.new(false)
  encoder = SonicPi::OSC::OscEncode.new(false)

  address = "/feeooblah"
  args = ["beans", 1, 2.0]

  Benchmark.ips do |bencher|
    bencher.report("osc") { encoder.encode_single_message(address, args) }
    bencher.compare
  end

  # Benchmark.ips do |bencher|
  #   bencher.report("samsosc") { samosc.decode_single_message(test_message) }
  #   bencher.report("oscruby") { oscruby.messages_from_network(test_message) }
  #
  #   bencher.compare
  # end
end
