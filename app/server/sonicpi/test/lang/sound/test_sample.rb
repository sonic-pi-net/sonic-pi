#--
# This file is part of Sonic Pi: http://sonic-pi.net
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

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/atom"
require_relative "../../../lib/sonicpi/buffer"
require_relative "../../../lib/sonicpi/lang/core"
require_relative "../../../lib/sonicpi/lang/sound"
require_relative "../../../lib/sonicpi/synths/synthinfo"
require 'mocha/setup'
require 'ostruct'

module SonicPi
  class SampleTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
      @lang.mod_sound_studio.stubs(:sample_loaded?).returns(true)
      @lang.stubs(:sample_find_candidates).returns(["/foo/bar.wav"])
    end

    def test_sample_with_various_args
      @lang.expects(:trigger_sampler).with("/foo/bar.wav",  {})
      @lang.run do
        sample :loop_amen
      end

      @lang.expects(:trigger_sampler).with("/foo/bar.wav", {rate: 2})
      @lang.run do
        sample :loop_amen, rate: 2
      end

      @lang.expects(:trigger_sampler).with("/foo/bar.wav", {rate: 2})
      @lang.run do
        sample lambda { :loop_amen }, rate: 2
      end

      #Single hash
      @lang.expects(:trigger_sampler).with("/foo/bar.wav", {rate: 2})
      @lang.run do
        sample path: :loop_amen, rate: 2
      end

      # Hash and args
      @lang.expects(:trigger_sampler).with("/foo/bar.wav", {rate: 2})
      @lang.run do
        sample({path: :loop_amen}, {rate: 2})
      end

      # Rates are not handled in a call to sample but are punted to trigger_sample
      @lang.expects(:trigger_sampler).with("/foo/bar.wav", {rate: 0})
      @lang.run do
        sample path: :loop_amen, rate: 0
      end

      # Rates are not handled in a call to sample but are punted to trigger_sample
      l = lambda{ 1 }
      @lang.expects(:trigger_sampler).with("/foo/bar.wav", {rate: l})
      @lang.run do
        sample path: :loop_amen, rate: l
      end

      # Onsets are not handled in a call to sample but are punted to trigger_sample
      @lang.expects(:trigger_sampler).with("/foo/bar.wav", {rate: 2, onset: 0})
      @lang.run do
        sample path: :loop_amen, rate: 2, onset: 0
      end
    end
  end
end
