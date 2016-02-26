# encoding: utf-8
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
  class SparkTester < Minitest::Test
    include SonicPi::Lang::Core

    def test_spark
      assert_equal("▁▃▅▇", spark_graph(1, 2, 3, 4))
      assert_equal("▁▃▅▇", spark_graph([1, 2, 3, 4]))
      assert_equal("▁", spark_graph(1))
      assert_equal("▁▁▁", spark_graph(3, 3, 3))
      assert_equal("", spark_graph([]))
      assert_equal("spark error: can't use nested arrays", spark_graph([1, 2], 3, 4))
      assert_equal("▁▁▁", spark_graph(false, false, false))
      assert_equal("▁▇▁", spark_graph(false, true, false))

    end
  end
end
