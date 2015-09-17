require_relative '../setup_test.rb'
require_relative "../../lib/sonicpi/oscdecode"
require 'benchmark/ips'
require 'osc-ruby'

samosc = SonicPi::OscDecode.new
oscruby = OSC::OSCPacket

test_message = OSC::Message.new("/foo", ["beans", 1, 2.0]).encode

Benchmark.ips do |bencher|
  bencher.report("samsosc") { samosc.decode_single_message(test_message) }
  bencher.report("oscruby") { oscruby.messages_from_network(test_message) }

  bencher.compare
end
