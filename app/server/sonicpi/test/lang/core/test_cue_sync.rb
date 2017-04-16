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
require_relative "../../../lib/sonicpi/lang/core"


module SonicPi
  class CueSyncTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end

    def test_basic_cue_with_no_args
      @lang.run do
        t = in_thread do
          res = sync :foo
          assert_equal [], res
        end

        cue :foo
        t.join
      end
    end


    def test_basic_cue_with_list_args
      @lang.run do
        t = in_thread do
          a, b, c = sync :foo
          assert_equal a, 1
          assert_equal b, "bar"
          assert_equal c, 3.38
        end

        cue :foo, 1, "bar", 3.38
        t.join
      end

      @lang.run do
        t = in_thread do
          res = sync :foo
          assert_equal res[0], 1
          assert_equal res[1], "bar"
          assert_equal res[2], 3.38
        end

        cue :foo, 1, "bar", 3.38
        t.join
      end
    end

    def test_basic_cue_with_map_args
      @lang.run do
        t = in_thread do
          a, b, c = sync :foo
          assert_equal a, 1
          assert_equal b, 2
          assert_equal c, 3.38
        end

        cue :foo, a: 1, b: 2, c: 3.38
        t.join
      end

      @lang.run do
        t = in_thread do
          res = sync :foo
          assert_equal res[:a], 1
          assert_equal res[:b], 2
          assert_equal res[:c], 3.38
        end

        cue :foo, a: 1, b: 2, c: 3.38
        t.join
      end
    end

    def test_multi_syncs
      @lang.run do
        t = in_thread do
          a, b, c = sync [:foo, :bar]
          assert_equal a, 1
          assert_equal b, 2
          assert_equal c, 3.38
        end

        cue :foo, a: 1, b: 2, c: 3.38
        t.join
      end
    end
  end
end
