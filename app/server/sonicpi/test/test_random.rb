require 'test/unit'
require_relative "../../core"
require_relative "../lib/sonicpi/spiderapi"

module SonicPi
  class RandomTester < Test::Unit::TestCase
    include SonicPi::SpiderAPI

    def test_rand
      rand_reset
      assert_equal(rand, 0.75006103515625)
      assert_equal(rand, 0.733917236328125)
      assert_equal(rand, 0.464202880859375)
      assert_equal(rand, 0.24249267578125)
    end

    def test_rand_reset
      rand_reset
      assert_equal(rand, 0.75006103515625)
      assert_equal(rand, 0.733917236328125)
      rand_reset
      assert_equal(rand, 0.75006103515625)
      assert_equal(rand, 0.733917236328125)
    end

    def test_rand_skip
      rand_reset
      assert_equal(rand, 0.75006103515625)
      rand_skip
      assert_equal(rand, 0.464202880859375)
    end

    def test_rand_skip_with_args
      rand_reset
      assert_equal(rand, 0.75006103515625)
      rand_skip(1)
      assert_equal(rand, 0.464202880859375)
      rand_skip(0)
      assert_equal(rand, 0.24249267578125)
    end

    def test_rand_multi_skip
      rand_reset
      assert_equal(rand, 0.75006103515625)
      rand_skip(2)
      assert_equal(rand, 0.24249267578125)
    end

    def test_rand_multi_skip2
      rand_reset
      99.times{rand}
      a = rand
      rand_reset
      rand_skip(99)
      assert_equal(rand, a)
    end

    def test_rand_back
      rand_reset
      a = rand
      b = rand
      c = rand
      rand_back
      assert_equal(rand, c)
      rand_back(2)
      assert_equal(rand, b)
      assert_equal(rand, c)
      rand_back(3)
      assert_equal(rand, a)
      assert_equal(rand, b)
      assert_equal(rand, c)
    end

    def test_rrand_handles_0_range
      assert_equal(1, rrand(1,1))
    end

    def test_rrand_i_handles_0_range
      assert_equal(1, rrand_i(1,1))
    end

    def test_rand_handles_0
      number = rand(0)
      assert(number >= 0 && number < 1)
    end

    def test_rand_i_handles_0
      number = rand_i(0)
      assert(number == 0)
    end

    def test_rand_only_returns_floats
      assert_equal(Float, rand(0..10).class)
      assert_equal(Float, rand(1).class)
    end

    def test_rand_i_only_returns_ints
      assert_equal(Fixnum, rand_i(0..10).class)
      assert_equal(Fixnum, rand_i(1.5).class)
    end
  end
end
