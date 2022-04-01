require_relative "./setup_test"
require_relative "../lib/sonicpi/util"
include SonicPi::Util
require_relative "../lib/sonicpi/metre/metre"

module SonicPi
  class MetreTester < Minitest::Test

    def test_flat
      four_four = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      ten_eight = Metre.new([[1/8r, 1/8r, 1/8r], [1/8r, 1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      eleven_eight = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], 1/8r, 3/4r])
      assert_equal(four_four.flat.to_s, "{1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8}")
      assert_equal(ten_eight.flat.to_s, "{1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8}")
      assert_equal(eleven_eight.flat.to_s, "{1/8+1/8+1/8+1/8+1/8+3/4}")
    end

    def test_fraction
      four_four = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      ten_eight = Metre.new([[1/8r, 1/8r, 1/8r], [1/8r, 1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      eleven_eight = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], 1/8r, 3/4r])
      assert_equal(four_four.fraction, 1/1r)
      assert_equal(ten_eight.fraction, 5/4r)
      assert_equal(eleven_eight.fraction, 11/8r)
    end

    def test_quarter_length
      four_four = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      ten_eight = Metre.new([[1/8r, 1/8r, 1/8r], [1/8r, 1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      eleven_eight = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], 1/8r, 3/4r])
      assert_equal(four_four.quarter_length, 4.0)
      assert_equal(ten_eight.quarter_length, 5.0)
      assert_equal(eleven_eight.quarter_length, 5.5)
    end

    def test_depth
      four_four = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      ten_eight = Metre.new([[1/8r, 1/8r, 1/8r], [1/8r, 1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      eleven_eight = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], 1/8r, 3/4r])
      assert_equal(four_four.depth, 2)
      assert_equal(ten_eight.depth, 2)
      assert_equal(eleven_eight.depth, 2)
    end

    def test_get_level
      four_four = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      ten_eight = Metre.new([[1/8r, 1/8r, 1/8r], [1/8r, 1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      eleven_eight = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], 1/8r, 3/4r])
      assert_equal(four_four.get_level(0).to_s, "{1/4+1/4+1/4+1/4}")
      assert_equal(ten_eight.get_level(0).to_s, "{3/8+3/8+1/4+1/4}")
      assert_equal(eleven_eight.get_level(0).to_s, "{1/4+1/4+1/8+3/4}")

      assert_equal(four_four.get_level(1).to_s, "{1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8}")
      assert_equal(ten_eight.get_level(1).to_s, "{1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8+1/8}")
      assert_equal(eleven_eight.get_level(1).to_s, "{1/8+1/8+1/8+1/8+1/16+1/16+3/8+3/8}")
    end

    def test_offset_to_index
      four_four = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      ten_eight = Metre.new([[1/8r, 1/8r, 1/8r], [1/8r, 1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      eleven_eight = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], 1/8r, 3/4r])
      assert_equal(four_four.offset_to_index(0), 0)
      assert_equal(ten_eight.offset_to_index(0), 0)
      assert_equal(eleven_eight.offset_to_index(0), 0)

      assert_equal(four_four.offset_to_index(1), 1)
      assert_equal(ten_eight.offset_to_index(1), 0)
      assert_equal(eleven_eight.offset_to_index(1), 1)

      assert_equal(four_four.offset_to_index(3.5), 3)
      assert_equal(ten_eight.offset_to_index(3.5), 2)
      assert_equal(eleven_eight.offset_to_index(3.5), 3)

      assert_raises RuntimeError do
        four_four.offset_to_index(4)
      end
      assert_raises RuntimeError do
        four_four.offset_to_index(10)
      end
    end

    def test_metrical_level_indices
      four_four = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      ten_eight = Metre.new([[1/8r, 1/8r, 1/8r], [1/8r, 1/8r, 1/8r], [1/8r, 1/8r], [1/8r, 1/8r]])
      eleven_eight = Metre.new([[1/8r, 1/8r], [1/8r, 1/8r], 1/8r, 3/4r])
      assert_equal(four_four.metrical_level_indices(0, 2), {0=>0, 1=>0, 2=>0})
      assert_equal(ten_eight.metrical_level_indices(0, 2), {0=>0, 1=>0, 2=>0})
      assert_equal(eleven_eight.metrical_level_indices(0, 2), {0=>0, 1=>0, 2=>0})

      assert_equal(four_four.metrical_level_indices(1, 2), {0=>1, 1=>2, 2=>4})
      assert_equal(ten_eight.metrical_level_indices(1, 2), {1=>2, 2=>4})
      assert_equal(eleven_eight.metrical_level_indices(1, 2), {0=>1, 1=>2, 2=>4})

      assert_equal(four_four.metrical_level_indices(3.5, 2), {1=>7, 2=>14})
      assert_equal(ten_eight.metrical_level_indices(3.5, 2), {1=>7, 2=>14})
      assert_equal(eleven_eight.metrical_level_indices(3.5, 2), {})
    end


  end
end
