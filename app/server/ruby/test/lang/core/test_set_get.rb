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
  class GetSetTester < Minitest::Test

    def setup
      @lang = SonicPi::MockLang.new
    end


    def test_time_increment
      # Remove for now - this is not currently deterministic
      # due to a lack of thread waiting

      # 10.times do
      #   @lang.run do
      #   use_bpm 800

      #     set :foo, 1
      #     set :foo, 2
      #     sleep 0.1

      #     t1 = in_thread do
      #       sleep 0
      #       assert_equal 2, get[:foo]
      #       set :foo, 3
      #     end

      #     # TODO: remove this when thread waiting
      #     # has been properly implemented
      #     t1.join

      #     assert_equal 2, get[:foo]

      #     t2 = in_thread do
      #       sleep 0
      #       set :foo, 5
      #       sleep 0.1
      #       sleep 0.1
      #       set :foo, 10
      #     end

      #     v = get(:foo)
      #     assert_equal 2, v

      #     sleep 0.1

      #     assert_equal 5, get[:foo]

      #     sleep 0.1

      #     assert_equal 5, get[:foo]
      #   end
      # end
    end

    def test_matchers
      @lang.run do
        set "/foo/bar3", 3
        assert_equal 3, get("/foo/bar?")
        assert_equal 3, get("/foo/bar{1,3}")
      end
    end
  end
end
