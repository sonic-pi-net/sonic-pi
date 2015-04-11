#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'test/unit'
require_relative "../../core"
require_relative "../lib/sonicpi/preparser"

module SonicPi
  module PreParser

    class PreParseError < StandardError ; end

    def self.preparse(rb)
      SonicPi::SpiderAPI.ring_fns.each do |fn|
        fn = fn[:name].to_s
        rb.gsub!(/\((\s*)#{fn}(\s)/, '\1' + fn + '(\2')
        raise PreParseError, "You may not use the built-in fn names as variable names.\n You attempted to use: #{fn}" if rb.match(/\W?#{fn}\s*=[\s\w]/)
      end
      rb
    end
  end
end

module SonicPi
  class PreParserTester < Test::Unit::TestCase
    def test_no_change
      a = "    def test_resolution_of_basic_major\n      assert_equal(Chord.new(:C4, :major), [60, 64, 67])\n      assert_equal(Chord.new(60, :major), [60, 64, 67])\n    end\n    end\n\n  end\nend"
      assert_equal(a, PreParser.preparse(a))
    end

    def test_basic_ring_change
      a = "(ring 50, 60, 70)"
      b = "ring( 50, 60, 70)"
      assert_equal(b, PreParser.preparse(a))
    end

    def test_raises_on_assignment_to_ring_fn
      a = "ring  = [50, 60, 70]"
      assert_raise PreParser::PreParseError do
        PreParser.preparse(a)
      end
    end
  end
end
