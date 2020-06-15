#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi
  class CueSyncTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end

    def test_in_thread
      @lang.run do
        assert_equal 0, vt

        t = in_thread do
          assert_equal 0, vt
          sleep 0.1
          assert_equal 0.1, vt
        end

        assert_equal 0, vt

        t.join
      end

    end


  end
end
