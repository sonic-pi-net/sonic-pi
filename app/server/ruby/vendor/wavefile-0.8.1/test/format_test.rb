require 'minitest/autorun'
require 'wavefile.rb'

include WaveFile

class FormatTest < Minitest::Test
  def test_valid_channels
    [1, 2, 3, 4, 65535].each do |valid_channels|
      assert_equal(valid_channels, Format.new(valid_channels, :pcm_16, 44100).channels)
    end

    assert_equal(1, Format.new(:mono, :pcm_16, 44100).channels)
    assert_equal(2, Format.new(:stereo, :pcm_16, 44100).channels)
  end

  def test_invalid_channels
    ["dsfsfsdf", :foo, 0, -1, 65536].each do |invalid_channels|
      assert_raises(InvalidFormatError) { Format.new(invalid_channels, :pcm_16, 44100) }
    end
  end

  def test_valid_sample_format
    assert_equal(:pcm, Format.new(:mono, :pcm_8, 44100).sample_format)
    assert_equal(:pcm, Format.new(:mono, :pcm_16, 44100).sample_format)
    assert_equal(:pcm, Format.new(:mono, :pcm_24, 44100).sample_format)
    assert_equal(:pcm, Format.new(:mono, :pcm_32, 44100).sample_format)
    assert_equal(:float, Format.new(:mono, :float, 44100).sample_format)
    assert_equal(:float, Format.new(:mono, :float_32, 44100).sample_format)
    assert_equal(:float, Format.new(:mono, :float_64, 44100).sample_format)
  end

  def test_invalid_sample_format
    ["dsfsfsdf", :foo, :pcm, 0, 12, :pcm_14, :pcm_abc, :float_40].each do |invalid_sample_format|
      assert_raises(InvalidFormatError) { Format.new(:mono, invalid_sample_format, 44100) }
    end
  end

  def test_valid_bits_per_sample
    assert_equal(8, Format.new(:mono, :pcm_8, 44100).bits_per_sample)
    assert_equal(16, Format.new(:mono, :pcm_16, 44100).bits_per_sample)
    assert_equal(24, Format.new(:mono, :pcm_24, 44100).bits_per_sample)
    assert_equal(32, Format.new(:mono, :pcm_32, 44100).bits_per_sample)
    assert_equal(32, Format.new(:mono, :float, 44100).bits_per_sample)
    assert_equal(32, Format.new(:mono, :float_32, 44100).bits_per_sample)
    assert_equal(64, Format.new(:mono, :float_64, 44100).bits_per_sample)
  end

  def test_valid_sample_rate
    [1, 44100, 4294967296].each do |valid_sample_rate|
      assert_equal(valid_sample_rate, Format.new(:mono, :pcm_16, valid_sample_rate).sample_rate)
    end
  end

  def test_invalid_sample_rate
    ["dsfsfsdf", :foo, 0, -1, 4294967297].each do |invalid_sample_rate|
      assert_raises(InvalidFormatError) { Format.new(:mono, :pcm_16, invalid_sample_rate) }
    end
  end

  def test_byte_and_block_align
    [1, :mono].each do |one_channel|
      format = Format.new(one_channel, :pcm_8, 44100)
      assert_equal(44100, format.byte_rate)
      assert_equal(1, format.block_align)

      format = Format.new(one_channel, :pcm_16, 44100)
      assert_equal(88200, format.byte_rate)
      assert_equal(2, format.block_align)

      format = Format.new(one_channel, :pcm_24, 44100)
      assert_equal(132300, format.byte_rate)
      assert_equal(3, format.block_align)

      [:pcm_32, :float, :float_32].each do |format_code|
        format = Format.new(one_channel, format_code, 44100)
        assert_equal(176400, format.byte_rate)
        assert_equal(4, format.block_align)
      end

      format = Format.new(one_channel, :float_64, 44100)
      assert_equal(352800, format.byte_rate)
      assert_equal(8, format.block_align) 
    end

    [2, :stereo].each do |two_channels|
      format = Format.new(two_channels, :pcm_8, 44100)
      assert_equal(88200, format.byte_rate)
      assert_equal(2, format.block_align)

      format = Format.new(two_channels, :pcm_16, 44100)
      assert_equal(176400, format.byte_rate)
      assert_equal(4, format.block_align)

      format = Format.new(two_channels, :pcm_24, 44100)
      assert_equal(264600, format.byte_rate)
      assert_equal(6, format.block_align)

      [:pcm_32, :float, :float_32].each do |format_code|
        format = Format.new(two_channels, format_code, 44100)
        assert_equal(352800, format.byte_rate)
        assert_equal(8, format.block_align)
      end

      format = Format.new(two_channels, :float_64, 44100)
      assert_equal(705600, format.byte_rate)
      assert_equal(16, format.block_align)
    end
  end

  def test_mono?
    [1, :mono].each do |one_channel|
      format = Format.new(one_channel, :pcm_8, 44100)
      assert_equal(true, format.mono?)
      assert_equal(false, format.stereo?)
    end
  end

  def test_stereo?
    [2, :stereo].each do |two_channels|
      format = Format.new(two_channels, :pcm_8, 44100)
      assert_equal(false, format.mono?)
      assert_equal(true, format.stereo?)
    end
  end
end
