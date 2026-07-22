#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/sonic-pi-net/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2025 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative './setup_test'

module SonicPi
  class BeautifyTester < Minitest::Test

    class Beautifier
      include SonicPi::RuntimeMethods
    end

    def setup
      @beautifier = Beautifier.new
    end

    def beautify(source)
      @beautifier.send(:beautify_ruby_source, source)
    end

    def test_indents_a_block
      assert_equal("live_loop :foo do\n  play 70\nend\n",
                   beautify("live_loop :foo do\nplay 70\nend\n"))
    end

    def test_appends_a_trailing_newline
      assert_equal("play 70\n", beautify("play 70"))
    end

    def test_leaves_already_formatted_source_alone
      formatted = "4.times do\n  sample :bd_haus\n  sleep 0.5\nend\n"
      assert_equal(formatted, beautify(formatted))
    end

    # A / directly after ) or ] must stay a division operator rather than being
    # read as the opening of a regex. See issue #2435.
    def test_division_after_closing_bracket_survives
      assert_equal("sleep (4)/2\n", beautify("sleep (4)/2\n"))
      assert_equal("sleep [4,8].choose/2\n", beautify("sleep [4,8].choose/2\n"))
    end
  end
end
