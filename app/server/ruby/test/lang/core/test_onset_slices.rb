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
require_relative "../../../lib/sonicpi/samplebuffer"

module SonicPi
  # Regression: `sample :loop_amen, onset: 0` raised
  #   undefined method '<<' for an instance of SonicPi::Core::RingVector
  # because SampleBuffer#onsets now returns a ring (map preserves ring type
  # since #3422) and onset_slices tried to mutate it with <<.
  class OnsetSlicesTester < Minitest::Test

    def buffer_with_onsets(onset_ring)
      sb = SonicPi::SampleBuffer.new(nil, "fake.flac")
      sb.define_singleton_method(:onsets) { |stretch = 1| onset_ring }
      sb
    end

    def test_onset_slices_works_when_onsets_is_a_ring
      sb = buffer_with_onsets([0.0, 0.25, 0.5].ring)
      slices = sb.onset_slices
      # a closing 1.0 bound is appended, giving 3 consecutive slices
      assert_equal [{ start: 0.0,  finish: 0.25, index: 0 },
                    { start: 0.25, finish: 0.5,  index: 1 },
                    { start: 0.5,  finish: 1.0,  index: 2 }],
                   slices.to_a
    end

    def test_onset_slices_handles_empty_onsets
      sb = buffer_with_onsets([].ring)
      # empty onsets => a single 0..1 slice, no crash
      assert_equal [{ start: 0, finish: 1, index: 0 }], sb.onset_slices.to_a
    end

    def test_onset_slices_does_not_mutate_the_source
      onsets = [0.0, 0.5].ring
      sb = buffer_with_onsets(onsets)
      sb.onset_slices
      assert_equal [0.0, 0.5], onsets.to_a, "onsets ring must not be mutated"
    end
  end
end
