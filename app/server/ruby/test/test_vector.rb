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
      v = [ 1, 2, 3 ].to_spv + [ 4, 5 ].to_spv
      assert_equal vector(1, 2, 3, 4, 5), v


      v2 = [ 1, 2, 3 ].to_spv + [ 4, 5 ]
      assert_equal vector(1, 2, 3, 4, 5), v2
    end

    def test_plus_other
      v = [ 1, 2, 3 ].to_spv + 10
      assert_equal(vector(11, 12, 13), v)

      v2 = [ 1, 2, 3 ].to_spv + 10.5
      assert_equal(vector(11.5, 12.5, 13.5), v2)
    end

    def test_minus_listy
      v = [ 1, 2, 3 ].to_spv - [ 4, 5, 1, 2 ].to_spv
      assert_equal vector(3), v

      v2 = [ 1, 2, 3 ].to_spv - [ 4, 5, 1, 2 ]
      assert_equal vector(3), v2
    end

    def test_minus_other
      v = [ 11, 12, 13 ].to_spv - 10
      assert_equal(vector(1, 2, 3), v)

      v2 = [ 11.5, 12.5, 13.5 ].to_spv - 10.5
      assert_equal(vector(1, 2, 3), v2)
    end

    def test_spaceship
      v1 = [ "a", "a", "c" ].to_spv
      v2 = [ "a", "b", "c" ].to_spv
      assert_equal(v1 <=> v2, -1)

      v1 = [ "a", "a", "c" ].to_spv
      v2 = [ "a", "b", "c" ]
      assert_equal(v1 <=> v2, -1)

      v3 = [ 1, 2, 3, 4, 5, 6 ].to_spv
      v4 = [ 1, 2 ].to_spv
      assert_equal(v3 <=> v4, 1)

      v3 = [ 1, 2, 3, 4, 5, 6 ].to_spv
      v4 = [ 1, 2 ]
      assert_equal(v3 <=> v4, 1)

      v5 = [ 1, 2 ].to_spv
      v6 = [ 1, :two ].to_spv
      assert_equal(v5 <=> v6, nil)

      v5 = [ 1, 2 ].to_spv
      v6 = [ 1, :two ]
      assert_equal(v5 <=> v6, nil)
    end

    def test_double_equals
      assert_not_equal([ "a", "c" ].to_spv, [ "a", "c", 7 ].to_spv)
      assert_not_equal([ "a", "c" ].to_spv, [ "a", "c", 7 ])
      assert_equal([ "a", "c", 7 ].to_spv,  [ "a", "c", 7].to_spv)
      assert_not_equal([ "a", "c", 7 ].to_spv,  [ "a", "d", "f"].to_spv)
    end

    def test_square_brackets
      a = [ "a", "b", "c", "d", "e" ].to_spv
      assert_equal(a[0], "a")
      assert_equal(a[1], "b")
      assert_equal(a[2], "c")

      assert_equal(a[6], nil)
      assert_equal(a[1, 2], [ "b", "c" ].to_spv)
      assert_equal(a[1..3], [ "b", "c", "d" ].to_spv)
      assert_equal(a[4..7], [ "e" ].to_spv)
      assert_equal(a[6..10], nil)
      assert_equal(a[-3, 3], [ "c", "d", "e" ].to_spv)

      assert_equal(a[5], nil)
      assert_equal(a[6, 1], nil)
      assert_equal(a[5, 1], [].to_spv)
      assert_equal(a[5..10], [].to_spv)
    end

    def test_all
      v = [1, 2, 3].to_spv
      assert(v.all? {|v| v > 0})
      assert_not(v.all? {|v| v < 0})
    end

    def test_any
      v = [1, 2, 3].to_spv
      assert(v.any? {|v| v > 0})
      assert_not(v.any? {|v| v > 3})
    end

    def test_compact
      a = [ "a", nil, "b", nil, "c", nil ].to_spv
      assert_equal(a.compact, [ "a", "b", "c" ].to_spv)
    end

    def test_each
      res = []
      v = [1, 2, 3].to_spv
      v.each do |el|
        res << el * 2
      end
      assert_equal([2, 4, 6], res)
    end

    def test_each_with_index
      res = []
      v = [1, 2, 3].to_spv
      v.each_with_index do |el, i|
        res << [el * 2, i]
      end
      assert_equal([[2, 0], [4, 1], [6, 2]], res)
    end

    def test_empty
      assert([].to_spv.empty?)
      assert_not([3].to_spv.empty?)
    end

    def test_eql
      v1 = [1, 2 ,3].to_spv
      v2 = [1, 2 ,3].to_spv
      v3 = [0, 2 ,3].to_spv
      assert(v1.eql?(v2))
      assert(v1.eql?(v1))
      assert_not(v1.eql?(v3))
    end

    def test_filter
      res = [1,2,3,4,5].to_spv.filter {|num| num.even? }
      assert_equal(res, [2, 4].to_spv)

      a = (%w[ a b c d e f ]).to_spv
      res2 = a.filter {|v| v =~ /[aeiou]/ }
      assert_equal(res2, ["a", "e"].to_spv)
    end

    def test_first
      assert_equal([1, 2, 3].to_spv.first, 1)
      assert_equal([1, 2, 3].to_spv.first(2), [1, 2].to_spv)
      assert_equal([1, 2, 3].to_spv.first(20), [1, 2, 3].to_spv)
    end

    def test_flatten
      s = [ 1, 2, 3 ].to_spv
      t = [ 4, 5, 6, [7, 8].to_spv ].to_spv
      a = [ s, t, 9, 10 ].to_spv
      res = a.flatten
      assert_equal(res, [1, 2, 3, 4, 5, 6, 7, 8, 9,10].to_spv)

      s2 = [ 1, 2, 3 ].to_spv
      t2 = [ 4, 5, 6, [7, 8] ].to_spv
      a2 = [ s2, t2, 9, 10 ].to_spv
      res2 = a2.flatten
      assert_equal(res2, [1, 2, 3, 4, 5, 6, 7, 8, 9,10].to_spv)

      a3 = [ 1, 2, [3, [4, 5].to_spv ].to_spv ].to_spv
      res3 = a3.flatten(1)
      assert_equal(res3, [1, 2, 3, [4, 5].to_spv].to_spv)

      s4 = [ 1, 2, 3 ].to_spv
      t4 = [ 4, 5, 6, [7, [8,8,8]] ].to_spv
      a4 = [ s4, t4, 9, 10 ].to_spv
      res4 = a4.flatten
      assert_equal(res4, [1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9,10].to_spv)

      s5 = [ 1, 2, 3 ].to_spv
      t5 = [ 4, 5, 6, [7, [8,8,[8, 8].to_spv]] ].to_spv
      a5 = [ s5, t5, 9, 10 ].to_spv
      res5 = a5.flatten(2)
      assert_equal(res5, [1, 2, 3, 4, 5, 6, 7, [8, 8, [8, 8].to_spv], 9,10].to_spv)
    end

    def test_flatten_within_standard_array
      s1 = [ 1, 2, 3 ].to_spv
      assert_equal(s1.to_a, [s1].flatten)
    end

    def test_flat_map
      res = [1, 2, 3, 4].to_spv.flat_map { |e| [e, -e].to_spv }
      assert_equal(res, [1, -1, 2, -2, 3, -3, 4, -4].to_spv)

      res2 = [1, 2, 3, 4].to_spv.flat_map { |e| [e, -e] }
      assert_equal(res2, [1, -1, 2, -2, 3, -3, 4, -4].to_spv)
#[[1, 2], [3, 4]].flat_map { |e| e + [100] } #=> [1, 2, 100, 3, 4, 100]
    end

    def test_index
      a = [ "a", "b", "c" ].to_spv
      assert_equal(a.index("b"), 1)
      assert_equal(a.index("z"), nil)
      assert_equal(a.index {|x| x == "b"}, 1)
    end

    def test_join
      assert_equal([ "a", "b", "c" ].to_spv.join, "abc")
      assert_equal([ "a", "b", "c" ].to_spv.join("-"), "a-b-c")
    end

    def test_last
      a = [ "w", "x", "y", "z" ].to_spv
      assert_equal(a.last, "z")
      assert_equal(a.last(2), ["y", "z"].to_spv)
    end

    def test_length
      vector = [3, 2, 1].to_spv
      assert_equal(3, vector.length)
    end

    def test_map
      res = []
      v = [1, 2, 3].to_spv
      v.map do |el|
        res << el * 2
      end
      assert_equal([2, 4, 6], res)
    end

    def test_max
      ary = (%w(albatross dog horse)).to_spv
      assert_equal(ary.max,  "horse")
      assert_equal(ary.max {|a, b| a.length <=> b.length}, "albatross")


      assert_equal(ary.max(2), ["horse", "dog"].to_spv)
      assert_equal(ary.max(2) {|a, b| a.length <=> b.length }, ["albatross", "horse"].to_spv)
    end

    def test_min
      ary = (%w(albatross dog horse)).to_spv
      assert_equal(ary.min,  "albatross")
      assert_equal(ary.min {|a, b| a.length <=> b.length}, "dog")


      assert_equal(ary.min(2), ["albatross", "dog"].to_spv)
      assert_equal(ary.min(2) {|a, b| a.length <=> b.length }, ["dog", "horse"].to_spv)
    end

    def test_reverse
      assert_equal([ "a", "b", "c" ].to_spv.reverse, ["c", "b", "a"].to_spv)
      assert_equal([ 1 ].to_spv.reverse, [1].to_spv)
    end

    def test_rotate
      a = [ "a", "b", "c", "d" ].to_spv
      assert_equal(a.rotate, ["b", "c", "d", "a"].to_spv)
      assert_equal(a,["a", "b", "c", "d"].to_spv)
      assert_equal(a.rotate(2), ["c", "d", "a", "b"].to_spv)
      assert_equal(a.rotate(-3),["b", "c", "d", "a"].to_spv)
    end

    def test_shuffle
      a = [ "a", "b", "c", "d", "e", "f", "g", "h", "i" ].to_spv * 20
      # hopefully this doesn't shuffle it to be the same ;-)
      assert_not_equal(a, a.shuffle)
      assert_equal(a.class, a.shuffle.class)
    end

    def test_sort
      v = [ "d", "a", "e", "c", "b" ].to_spv
      assert_equal(v.sort, ["a", "b", "c", "d", "e"].to_spv)
      assert_equal(v.sort {|a, b| b <=> a}, ["e", "d", "c", "b", "a"].to_spv)
    end

    def test_take
      a = [1, 2, 3, 4, 5, 0].to_spv
      assert_equal(a.take(3), [1, 2, 3].to_spv)
      assert_equal(a.take(-3), [0, 5, 4].to_spv)
      assert_equal([].to_spv.take(-3), [].to_spv)

      assert_equal([1, 2, 3].to_spv.take(7), [1, 2, 3, 1, 2, 3, 1].to_spv)
    end

    def test_drop
      a = [1, 2, 3, 4, 5, 0].to_spv
      assert_equal(a.drop(3), [4, 5, 0].to_spv)
      assert_equal(a.drop(30), [].to_spv)
    end

    def test_drop_last
      a = [1, 2, 3, 4, 5, 0].to_spv
      assert_equal(a.drop_last, [1, 2, 3, 4, 5].to_spv)
      assert_equal(a.drop_last(3), [1, 2, 3].to_spv)
    end

    def test_size
      assert_equal([1, 2, 3, 4].to_spv.size, 4)
    end

    def test_uniq
      a = [ "a", "a", "b", "b", "c" ].to_spv
      assert_equal(a.uniq, ["a", "b", "c"].to_spv)

      b = [["student","sam"].to_spv, ["student","george"].to_spv, ["teacher","matz"].to_spv].to_spv
      assert_equal(b.uniq {|s| s.first},  [["student", "sam"].to_spv, ["teacher", "matz"].to_spv].to_spv)
    end

    def test_value_at
      a = (%w{ a b c d e f }).to_spv
      assert_equal(["b", "d", "f"].to_spv, a.values_at(1, 3, 5))
      assert_equal(["b", "d", "f", nil].to_spv, a.values_at(1, 3, 5, 7))
      assert_equal(["f", "e", "e", nil].to_spv, a.values_at(-1, -2, -2, -7))
      assert_equal(["e", "f", nil, "d", "e", "f"].to_spv, a.values_at(4..6, 3...6))
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
      v = [:a, :b, []].to_spv
      v2 = v.__sp_make_thread_safe

      assert !v.sp_thread_safe?

      assert v == v2

      assert v2.sp_thread_safe?
      assert v2[2].frozen?
    end

  end
end
