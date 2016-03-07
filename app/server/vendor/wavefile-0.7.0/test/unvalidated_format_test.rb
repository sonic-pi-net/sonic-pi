require 'minitest/autorun'
require 'wavefile.rb'

include WaveFile

class UnvalidatedFormatTest < MiniTest::Unit::TestCase
  def test_initialize
    format = UnvalidatedFormat.new({:audio_format => 1,
                                    :channels => 2,
                                    :sample_rate => 44100,
                                    :byte_rate => 176400,
                                    :block_align => 4,
                                    :bits_per_sample => 16})

    assert_equal(1,      format.audio_format)
    assert_equal(2,      format.channels)
    assert_equal(false,  format.mono?)
    assert_equal(true,   format.stereo?)
    assert_equal(44100,  format.sample_rate)
    assert_equal(176400, format.byte_rate)
    assert_equal(4,      format.block_align)
    assert_equal(16,     format.bits_per_sample)
  end
end
