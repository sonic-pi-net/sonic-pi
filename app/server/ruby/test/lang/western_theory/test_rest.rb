#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2020  by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "../../setup_test"

module SonicPi
  module Lang
    module WesternTheory
      class RestTester < Minitest::Test
        def setup
          @lang = SonicPi::MockLang.new
        end

        def test_rest
          @lang.run do
            assert_equal(false, rest?(1))
            assert_equal(true, rest?(:rest))
            assert_equal(true, rest?(:r))
            assert_equal(false, rest?(:norest))
            assert_equal(true, rest?(nil))
            assert_equal(false, rest?(Hash.new))
            assert_equal(false, rest?("a string"))
          end
        end
      end
    end
  end
end
