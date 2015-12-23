require 'test/unit'
require 'wavefile.rb'

include WaveFile

class InfoTest < Test::Unit::TestCase
  FILE_NAME = "foo.wav"
  SECONDS_IN_MINUTE = 60
  SECONDS_IN_HOUR = SECONDS_IN_MINUTE * 60

  def test_basic
    format_chunk = { :audio_format => 1, :channels => 2, :sample_rate => 44100,
                     :byte_rate => 176400, :block_align => 4, :bits_per_sample => 16 }
    info = Info.new(FILE_NAME, format_chunk, 44100)

    assert_equal(FILE_NAME, info.file_name)
    assert_equal(1, info.audio_format)
    assert_equal(2, info.channels)
    assert_equal(44100, info.sample_rate)
    assert_equal(176400, info.byte_rate)
    assert_equal(4, info.block_align)
    assert_equal(16, info.bits_per_sample)
    assert_equal(44100, info.sample_frame_count)

    assert_equal(0, info.duration.hours)
    assert_equal(0, info.duration.minutes)
    assert_equal(1, info.duration.seconds)
    assert_equal(0, info.duration.milliseconds)
    assert_equal(44100, info.duration.sample_frame_count)
    assert_equal(44100, info.duration.sample_rate)
  end
end
