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
  # Regression for #3422: .map on a ring/ramp/vector must return the same
  # type (like flat_map/flatten/reverse/rotate), not degrade to a plain Array,
  # so ring-ness (wrapped/clamped indexing, tick) survives a map.
  class VectorMapTester < Minitest::Test

    def test_ring_map_returns_a_ring
      mapped = [1, 2, 3].ring.map { |x| x * 2 }
      assert_instance_of SonicPi::Core::RingVector, mapped
      assert_equal [2, 4, 6], mapped.to_a
      assert_equal 2, mapped[3]     # ring index wraps: 3 % 3 => 0
    end

    def test_ramp_map_returns_a_ramp
      mapped = [1, 2, 3].ramp.map { |x| x * 2 }
      assert_instance_of SonicPi::Core::RampVector, mapped
      assert_equal [2, 4, 6], mapped.to_a
      assert_equal 6, mapped[10]    # ramp index clamps to the last element
    end

    def test_map_still_transforms_the_values
      assert_equal [1, 4, 9], [1, 2, 3].ring.map { |x| x * x }.to_a
    end

    def test_map_now_matches_flat_map_type
      r = [1, 2, 3].ring
      assert_instance_of SonicPi::Core::RingVector, r.map { |x| x }
      assert_instance_of SonicPi::Core::RingVector, r.flat_map { |x| [x] }
    end
  end
end
