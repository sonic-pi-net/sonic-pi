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

require_relative "./setup_test"
require_relative "../lib/sonicpi/lang/core"

module SonicPi
  class VectorTester < Minitest::Test
    include SonicPi::Lang::Core

    def test_index
      v = vector(:a, :b, :c)
      assert_equal(v[0], :a)
      assert_equal(v[-1], :c)
      assert_equal(v[-100], nil)
      assert_equal(v[100], nil)
    end

    def test_init
      v = vector(:a, :b, :c)
      assert v.sp_thread_safe?
    end

    def test_init_w_non_ts_vals
      v = vector(:a, :b, [])
      v2 = v.__sp_make_thread_safe

      assert !v.sp_thread_safe?

      assert v == v2

      assert v2.sp_thread_safe?
      assert v2[2].frozen?
    end

  end
end
