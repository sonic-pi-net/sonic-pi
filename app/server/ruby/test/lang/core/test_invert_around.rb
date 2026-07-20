#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2026 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi
  # #2089: (ring ...).invert_around(pitch) reflects each element around the
  # pitch axis (melodic inversion), so 2*axis - note.
  class InvertAroundTester < Minitest::Test

    def test_reflects_around_a_numeric_axis
      assert_equal [60, 56, 53], [60, 64, 67].ring.invert_around(60).to_a
    end

    def test_resolves_symbolic_notes_and_axis
      assert_equal [60, 56, 53], [:c4, :e4, :g4].ring.invert_around(:c4).to_a
    end

    def test_rests_pass_through
      assert_equal [60, nil, 56], [60, :r, 64].ring.invert_around(60).to_a
    end

    def test_returns_a_ring
      assert_instance_of SonicPi::Core::RingVector, [60, 64].ring.invert_around(60)
    end

    def test_a_rest_axis_raises
      assert_raises(RuntimeError) { [60, 64].ring.invert_around(:r) }
    end
  end
end
