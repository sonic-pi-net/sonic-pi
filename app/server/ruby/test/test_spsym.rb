#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/preparser"

module SonicPi
  class SPSymTester < Minitest::Test
    def test_sanity
      assert_equal 0, 0
    end

    def test_basic_properties
      a = eval(PreParser.preparse(":foo:bar",  SonicPi::Lang::Core.vec_fns))
      b = eval(PreParser.preparse(":foo:bar",  SonicPi::Lang::Core.vec_fns))

      assert_equal a, b
      assert_equal a.object_id, b.object_id
      assert_equal ":foo:bar", a.inspect
      assert_equal ":foo:bar", b.inspect

      assert_equal [:foo, :bar], b.path
    end
  end
end
