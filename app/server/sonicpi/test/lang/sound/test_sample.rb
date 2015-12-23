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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/atom"
require_relative "../../../lib/sonicpi/buffer"
require_relative "../../../lib/sonicpi/lang/core"
require_relative "../../../lib/sonicpi/lang/sound"
require 'mocha/setup'
require 'ostruct'

module SonicPi
  class SampleTester < Minitest::Test
    def setup
      @mock_sound = Object.new
      @mock_sound.extend(Lang::Sound)
      @mock_sound.extend(Lang::Core)
      @mock_sound.stubs(:sleep) # avoid loading Spider class
      @mock_sound.stubs(:ensure_good_timing!) # avoid loading Spider class
      @mock_sound.stubs(:load_sample).returns(OpenStruct.new({id: 42, num_chans: 2}))
    end

    def test_sample_with_various_args
      @mock_sound.expects(:trigger_sampler).with(:loop_amen, 42, 2, {})
      @mock_sound.sample :loop_amen

      @mock_sound.expects(:trigger_sampler).with(:loop_amen, 42, 2, {rate: 2})
      @mock_sound.sample :loop_amen, rate: 2

      @mock_sound.expects(:trigger_sampler).with(:loop_amen, 42, 2, {rate: 2})
      @mock_sound.sample lambda { :loop_amen }, rate: 2

      # Single hash
      @mock_sound.expects(:trigger_sampler).with(:loop_amen, 42, 2, {rate: 2})
      @mock_sound.sample name: :loop_amen, rate: 2

      # Hash and args
      @mock_sound.expects(:trigger_sampler).with(:loop_amen, 42, 2, {rate: 2})
      @mock_sound.sample({name: :loop_amen}, {rate: 2})
    end

  end
end
