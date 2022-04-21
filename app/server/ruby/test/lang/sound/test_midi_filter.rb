# #--
# # This file is part of Sonic Pi: http://sonic-pi.net
# # Full project source: https://github.com/samaaron/sonic-pi
# # License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
# #
# # Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# # All rights reserved.
# #
# # Permission is granted for use, copying, modification, and
# # distribution of modified versions of this work as long as this
# # notice is included.
# #++

# require_relative "../../setup_test"
# require_relative "../../../lib/sonicpi/lang/core"

# require 'mocha/setup'

# module SonicPi
#   class SonicPiMiniTest < MiniTest::Test

#     def setup
#       @lang = SonicPi::MockLang.new

#       @midi_ports = [
#         "E-MU Xmidi 2*2 Midi In 1",
#         "E-MU Xmidi 2*2 Midi In 2" ].map{|el| el.freeze}
#       @lang.stubs(:midi_available_ports).returns(@midi_ports)
#     end

#     def test_sanity
#       @lang.run do
#         assert_equal(1, 1)
#       end
#     end

#     def test_available_ports
#       ports = @midi_ports
#       @lang.run do
#         assert_equal(ports, midi_available_ports)
#       end
#     end

#     def test_current_midi_ports_defaults_to_star
#       @lang.run do
#         assert_equal(['*'], current_midi_ports)
#       end
#     end

#     def test_set_current_midi_ports_with_filter
#       ports = @midi_ports
#       @lang.run do
#         use_midi_ports "Midi In 1"
#         assert_equal(ports.take(1), current_midi_ports)

#         use_midi_ports "Midi In 2"
#         assert_equal(ports.drop(1).take(1), current_midi_ports)

#         use_midi_ports "2*2 Midi In 2"
#         assert_equal(ports.drop(1).take(1), current_midi_ports)

#         use_midi_ports "2*2", "Midi", "2"
#         assert_equal(ports.drop(1).take(1), current_midi_ports)

#         use_midi_ports /2\Z/
#         assert_equal(ports.drop(1).take(1), current_midi_ports)

#         use_midi_ports "E-MU"
#         assert_equal(ports, current_midi_ports)

#         use_midi_ports "E-MU", 0
#         assert_equal(ports.take(1), current_midi_ports)

#         use_midi_ports "E-MU", 1
#         assert_equal(ports.drop(1).take(1), current_midi_ports)

#         use_midi_ports "*"
#         assert_equal('*', current_midi_ports)
#       end
#     end
#   end
# end
