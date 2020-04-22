require_relative "../../setup_test"
require_relative "../../../lib/sonicpi/lang/core"

module SonicPi
  class RandomTester < Minitest::Test
    include SonicPi::Lang::Core

    def test_rand
      use_random_stream :white
      rand_reset
      assert_equal(rand, 0.75006103515625)
      assert_equal(rand, 0.733917236328125)
      assert_equal(rand, 0.464202880859375)
      assert_equal(rand, 0.24249267578125)
    end

    def test_rand_type
      use_random_stream :white
      rand_reset
      assert_equal(rand, 0.75006103515625)
      assert_equal(rand, 0.733917236328125)
      use_random_stream :white
      rand_reset
      assert_equal(rand, 0.75006103515625)
      assert_equal(rand, 0.733917236328125)
      use_random_stream :pink
      rand_reset
      assert_equal(rand, 0.47808837890625)
      assert_equal(rand, 0.56011962890625)
      use_random_stream :light_pink
      rand_reset
      assert_equal(rand, 0.53851318359375)
      assert_equal(rand, 0.54705810546875)
      use_random_stream :dark_pink
      rand_reset
      assert_equal(rand, 0.442596435546875)
      assert_equal(rand, 0.443756103515625)
      use_random_stream :perlin
      rand_reset
      assert_equal(rand, 0.546478271484375)
      assert_equal(rand, 0.573150634765625)
      with_random_stream :white do
        # matches 3rd value from test_rand
        # index is not reset when changing type
        assert_equal(rand, 0.464202880859375)
      end

      # last stream type was perlin
      assert_equal(rand, 0.597015380859375)

      # return to default
      use_random_stream :white
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
      assert_equal(Integer, rand_i(0..10).class)
      assert_equal(Integer, rand_i(1.5).class)
    end

    def test_rand_look
      rand_reset
      assert_equal(rand_look, 0.75006103515625)

      rand_reset
      assert_equal(rand_look(0.5), 0.375030517578125)
    end

    def test_rand_i_look
      rand_reset
      assert_equal(rand_i_look, 1)

      rand_reset
      assert_equal(rand_i_look(100), 75)
    end
  end
end
