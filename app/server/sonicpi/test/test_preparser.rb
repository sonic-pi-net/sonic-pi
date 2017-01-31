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
require_relative "../lib/sonicpi/preparser"

module SonicPi
  class PreParserTester < Minitest::Test
    def test_no_change
      a = "    def test_resolution_of_basic_major\n      assert_equal(Chord.new(:C4, :major), [60, 64, 67])\n      assert_equal(Chord.new(60, :major), [60, 64, 67])\n    end\n    end\n\n  end\nend"
      assert_equal(a, PreParser.preparse(a,  SonicPi::Lang::Core.vec_fns))
    end

    def test_basic_ring_change_w_frozen_string
      a = "(ring 50, 60, 70)".freeze
      b = " ring(50, 60, 70)"
      assert_equal(b, PreParser.preparse(a,  SonicPi::Lang::Core.vec_fns))
    end

    def test_basic_ring_change
      a = "(ring 50, 60, 70)"
      b = " ring(50, 60, 70)"
      assert_equal(b, PreParser.preparse(a,  SonicPi::Lang::Core.vec_fns))
    end

    def test_basic_ring_change_with_leading_space
      a = "(  ring 50, 60, 70)"
      b = "   ring(50, 60, 70)"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end

    def test_basic_rings_with_commas
      a = "(ring, 50, 60, 70)"
      b = " ring( 50, 60, 70)"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end

    def test_basic_rings_with_commas_and_no_space
      a = "(ring,50, 60, 70)"
      b = " ring(50, 60, 70)"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end

    def test_spaced_rings_with_commas
      a = "(ring , 50, 60, 70)"
      b = " ring(  50, 60, 70)"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end

    def test_raises_on_assignment_to_ring_fn
      a = "ring  = [50, 60, 70]"
      assert_raises PreParser::PreParseError do
        PreParser.preparse(a, SonicPi::Lang::Core.vec_fns)
      end
    end

    def test_partial_matches_on_builtin_fns
      a = "testscale = 10"
      # minitest doesn't have a refute_raises
      assert_equal(a, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end

    def test_using_a_builtin_raises_an_exception
      a = "scale = 10"
      assert_raises PreParser::PreParseError do
        PreParser.preparse(a, SonicPi::Lang::Core.vec_fns)
      end
    end

    def test_sp_sym_basic_expansion
      a = "foo :baz:quux eggs"
      b = "foo ::SonicPi::SPSym.new('baz : quux') eggs"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end

    def test_sp_sym_multiterm_expansion
      a = "foo :baz:quux:bar:beans eggs"
      b = "foo ::SonicPi::SPSym.new('baz : quux : bar : beans') eggs"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end

    def test_sp_sym_complex_expansion
      a = "foo :baz?:quux eggs"
      b = "foo ::SonicPi::SPSym.new('baz? : quux') eggs"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))


      a = "foo :baz?:qu_ux eggs"
      b = "foo ::SonicPi::SPSym.new('baz? : qu_ux') eggs"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))

      a = "foo :_b_az?:qu_ux eggs :beans"
      b = "foo ::SonicPi::SPSym.new('_b_az? : qu_ux') eggs :beans"
      assert_equal(b, PreParser.preparse(a, SonicPi::Lang::Core.vec_fns))
    end
  end
end
