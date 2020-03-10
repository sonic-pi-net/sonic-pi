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

    def test_amper
      v1 = vector( 1, 1, 3, 5)
      v2 = vector( 3, 2, 1)
      a2 = [3, 2, 1]
      v3 = vector(1, 3)
      assert_equal(v1 & v2, v3)
      assert_equal(v1 & a2, v3)
    end

    def test_plus_listy
      v = [ 1, 2, 3 ].to_v + [ 4, 5 ].to_v
      assert_equal vector(1, 2, 3, 4, 5), v


      v2 = [ 1, 2, 3 ].to_v + [ 4, 5 ]
      assert_equal vector(1, 2, 3, 4, 5), v2
    end

    def test_plus_other
      v = [ 1, 2, 3 ].to_v + 10
      assert_equal(vector(11, 12, 13), v)

      v2 = [ 1, 2, 3 ].to_v + 10.5
      assert_equal(vector(11.5, 12.5, 13.5), v2)
    end

    def test_minus_listy
      v = [ 1, 2, 3 ].to_v - [ 4, 5, 1, 2 ].to_v
      assert_equal vector(3), v

      v2 = [ 1, 2, 3 ].to_v - [ 4, 5, 1, 2 ]
      assert_equal vector(3), v2
    end

    def test_minus_other
      v = [ 11, 12, 13 ].to_v - 10
      assert_equal(vector(1, 2, 3), v)

      v2 = [ 11.5, 12.5, 13.5 ].to_v - 10.5
      assert_equal(vector(1, 2, 3), v2)
    end

    def test_spaceship
      v1 = [ "a", "a", "c" ].to_v
      v2 = [ "a", "b", "c" ].to_v
      assert_equal(v1 <=> v2, -1)

      v1 = [ "a", "a", "c" ].to_v
      v2 = [ "a", "b", "c" ]
      assert_equal(v1 <=> v2, -1)

      v3 = [ 1, 2, 3, 4, 5, 6 ].to_v
      v4 = [ 1, 2 ].to_v
      assert_equal(v3 <=> v4, 1)

      v3 = [ 1, 2, 3, 4, 5, 6 ].to_v
      v4 = [ 1, 2 ]
      assert_equal(v3 <=> v4, 1)

      v5 = [ 1, 2 ].to_v
      v6 = [ 1, :two ].to_v
      assert_equal(v5 <=> v6, nil)

      v5 = [ 1, 2 ].to_v
      v6 = [ 1, :two ]
      assert_equal(v5 <=> v6, nil)
    end

    def test_double_equals
      assert_not_equal([ "a", "c" ].to_v, [ "a", "c", 7 ].to_v)
      assert_not_equal([ "a", "c" ].to_v, [ "a", "c", 7 ])
      assert_equal([ "a", "c", 7 ].to_v,  [ "a", "c", 7].to_v)
      assert_not_equal([ "a", "c", 7 ].to_v,  [ "a", "d", "f"].to_v)
    end

    def test_square_brackets
      a = [ "a", "b", "c", "d", "e" ].to_v
      assert_equal(a[0], "a")
      assert_equal(a[1], "b")
      assert_equal(a[2], "c")

      assert_equal(a[6], nil)
      assert_equal(a[1, 2], [ "b", "c" ].to_v)
      assert_equal(a[1..3], [ "b", "c", "d" ].to_v)
      assert_equal(a[4..7], [ "e" ].to_v)
      assert_equal(a[6..10], nil)
      assert_equal(a[-3, 3], [ "c", "d", "e" ].to_v)

      assert_equal(a[5], nil)
      assert_equal(a[6, 1], nil)
      assert_equal(a[5, 1], [].to_v)
      assert_equal(a[5..10], [].to_v)
    end

    def test_all
      v = [1, 2, 3].to_v
      assert(v.all? {|v| v > 0})
      assert_not(v.all? {|v| v < 0})
    end

    def test_any
      v = [1, 2, 3].to_v
      assert(v.any? {|v| v > 0})
      assert_not(v.any? {|v| v > 3})
    end

    def test_compact
      a = [ "a", nil, "b", nil, "c", nil ].to_v
      assert_equal(a.compact, [ "a", "b", "c" ].to_v)
    end

    def test_each
      res = []
      v = [1, 2, 3].to_v
      v.each do |el|
        res << el * 2
      end
      assert_equal([2, 4, 6], res)
    end

    def test_empty
      assert([].to_v.empty?)
      assert_not([3].to_v.empty?)
    end

    def test_eql
      v1 = [1, 2 ,3].to_v
      v2 = [1, 2 ,3].to_v
      v3 = [0, 2 ,3].to_v
      assert(v1.eql?(v2))
      assert(v1.eql?(v1))
      assert_not(v1.eql?(v3))
    end

    def test_filter
      res = [1,2,3,4,5].to_v.filter {|num| num.even? }
      assert_equal(res, [2, 4].to_v)

      a = (%w[ a b c d e f ]).to_v
      res2 = a.filter {|v| v =~ /[aeiou]/ }
      assert_equal(res2, ["a", "e"].to_v)
    end

    def test_first
      assert_equal([1, 2, 3].to_v.first, 1)
      assert_equal([1, 2, 3].to_v.first(2), [1, 2].to_v)
      assert_equal([1, 2, 3].to_v.first(20), [1, 2, 3].to_v)
    end

    # def test_flatten
    #   s = [ 1, 2, 3 ].to_v
    #   t = [ 4, 5, 6, [7, 8].to_v ].to_v
    #   a = [ s, t, 9, 10 ].to_v
    #   res = a.flatten
    #   assert_equal(res, [1, 2, 3, 4, 5, 6, 7, 8, 9,10].to_v)
    #   a2= [ 1, 2, [3, [4, 5].to_v ].to_v ].to_v
    #   res2 = a2.flatten(1)
    #   assert_equal(res2, [1, 2, 3, [4, 5]])
    # end

    def test_index
      a = [ "a", "b", "c" ].to_v
      assert_equal(a.index("b"), 1)
      assert_equal(a.index("z"), nil)
      assert_equal(a.index {|x| x == "b"}, 1)
    end

    def test_join
      assert_equal([ "a", "b", "c" ].to_v.join, "abc")
      assert_equal([ "a", "b", "c" ].to_v.join("-"), "a-b-c")
    end

    def test_last
      a = [ "w", "x", "y", "z" ].to_v
      assert_equal(a.last, "z")
      assert_equal(a.last(2), ["y", "z"].to_v)
    end

    def test_map
      res = []
      v = [1, 2, 3].to_v
      v.map do |el|
        res << el * 2
      end
      assert_equal([2, 4, 6], res)
    end

    def test_max
      ary = (%w(albatross dog horse)).to_v
      assert_equal(ary.max,  "horse")
      assert_equal(ary.max {|a, b| a.length <=> b.length}, "albatross")


      assert_equal(ary.max(2), ["horse", "dog"].to_v)
      assert_equal(ary.max(2) {|a, b| a.length <=> b.length }, ["albatross", "horse"].to_v)
    end

    def test_min
      ary = (%w(albatross dog horse)).to_v
      assert_equal(ary.min,  "albatross")
      assert_equal(ary.min {|a, b| a.length <=> b.length}, "dog")


      assert_equal(ary.min(2), ["albatross", "dog"].to_v)
      assert_equal(ary.min(2) {|a, b| a.length <=> b.length }, ["dog", "horse"].to_v)
    end

    def test_reverse
      assert_equal([ "a", "b", "c" ].to_v.reverse, ["c", "b", "a"].to_v)
      assert_equal([ 1 ].to_v.reverse, [1].to_v)
    end

    def test_rotate
      a = [ "a", "b", "c", "d" ].to_v
      assert_equal(a.rotate, ["b", "c", "d", "a"].to_v)
      assert_equal(a,["a", "b", "c", "d"].to_v)
      assert_equal(a.rotate(2), ["c", "d", "a", "b"].to_v)
      assert_equal(a.rotate(-3),["b", "c", "d", "a"].to_v)
    end

    def test_shuffle
      a = [ "a", "b", "c", "d", "e", "f", "g", "h", "i" ].to_v * 20
      # hopefully this doesn't shuffle it to be the same ;-)
      assert_not_equal(a, a.shuffle)
      assert_equal(a.class, a.shuffle.class)
    end

    def test_sort
      v = [ "d", "a", "e", "c", "b" ].to_v
      assert_equal(v.sort, ["a", "b", "c", "d", "e"].to_v)
      assert_equal(v.sort {|a, b| b <=> a}, ["e", "d", "c", "b", "a"].to_v)
    end

    def test_take
      a = [1, 2, 3, 4, 5, 0].to_v
      assert_equal(a.take(3), [1, 2, 3].to_v)
      assert_equal(a.take(-3), [0, 5, 4].to_v)
      assert_equal([].to_v.take(-3), [].to_v)

      assert_equal([1, 2, 3].to_v.take(7), [1, 2, 3, 1, 2, 3, 1].to_v)
    end

    def test_drop
      a = [1, 2, 3, 4, 5, 0].to_v
      assert_equal(a.drop(3), [4, 5, 0].to_v)
      assert_equal(a.drop(30), [].to_v)
    end

    def test_drop_last
      a = [1, 2, 3, 4, 5, 0].to_v
      assert_equal(a.drop_last, [1, 2, 3, 4, 5].to_v)
      assert_equal(a.drop_last(3), [1, 2, 3].to_v)
    end

    def test_size
      assert_equal([1, 2, 3, 4].to_v.size, 4)
    end

    def test_uniq
      a = [ "a", "a", "b", "b", "c" ].to_v
      assert_equal(a.uniq, ["a", "b", "c"].to_v)

      b = [["student","sam"].to_v, ["student","george"].to_v, ["teacher","matz"].to_v].to_v
      assert_equal(b.uniq {|s| s.first},  [["student", "sam"].to_v, ["teacher", "matz"].to_v].to_v)
    end



    def test_star
      v = vector(:a, :b, :c)
      v2 = vector(:a, :b, :c, :a, :b, :c)
      assert_equal(v * 2, v2)
    end



    def test_init
      v = vector(:a, :b, :c)
      assert v.sp_thread_safe?
    end

    def test_init_w_non_ts_vals
      v = [:a, :b, []].to_v
      v2 = v.__sp_make_thread_safe

      assert !v.sp_thread_safe?

      assert v == v2

      assert v2.sp_thread_safe?
      assert v2[2].frozen?
    end

  end
end
