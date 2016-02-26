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
require_relative "../lib/sonicpi/util"

module SonicPi

  class UtilTester < Minitest::Test
    include Util
    def test_merge_synth_arg_maps_array
      a = [{foo: 1, bar: 2, baz: 4}, {eggs: 5}, beans: 6, foo: 7]
      r = {foo: 7, bar: 2, baz: 4, eggs: 5, beans: 6}
      assert_equal(r, merge_synth_arg_maps_array(a))

      a = [{foo: 1, bar: 2, baz: 4}, {eggs: 5}]
      r = {foo: 1, bar: 2, baz: 4, eggs: 5}
      assert_equal(r, merge_synth_arg_maps_array(a))

      a = [{foo: 1, bar: 2, baz: 4}]
      r = {foo: 1, bar: 2, baz: 4}
      assert_equal(r, merge_synth_arg_maps_array(a))
    end
  end
end
