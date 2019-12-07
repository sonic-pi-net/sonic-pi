require 'minitest/autorun'
require 'wavefile.rb'

include WaveFile

class UnvalidatedFormatTest < Minitest::Test
  def test_initialize
    format = UnvalidatedFormat.new({:audio_format => 65534,
                                    :sub_audio_format_guid => SUB_FORMAT_GUID_PCM,
                                    :channels => 2,
                                    :sample_rate => 44100,
                                    :byte_rate => 176400,
                                    :block_align => 4,
                                    :bits_per_sample => 16,
                                    :valid_bits_per_sample => 14})

    assert_equal(65534,  format.audio_format)
    assert_equal(SUB_FORMAT_GUID_PCM, format.sub_audio_format_guid)
    assert_equal(2,      format.channels)
    assert_equal(false,  format.mono?)
    assert_equal(true,   format.stereo?)
    assert_equal(44100,  format.sample_rate)
    assert_equal(176400, format.byte_rate)
    assert_equal(4,      format.block_align)
    assert_equal(16,     format.bits_per_sample)
    assert_equal(14,     format.valid_bits_per_sample)
  end

  def test_to_validated_format_pcm
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 1,
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 176400,
                                                :block_align => 4,
                                                :bits_per_sample => 16})

    validated_format = unvalidated_format.to_validated_format
    assert_equal(:pcm, validated_format.sample_format)
    assert_equal(2, validated_format.channels)
    assert_equal(16, validated_format.bits_per_sample)
    assert_equal(44100, validated_format.sample_rate)
    assert_equal(176400, validated_format.byte_rate)
    assert_equal(4, validated_format.block_align)
  end

  def test_to_validated_format_float
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 3,
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 352800,
                                                :block_align => 8,
                                                :bits_per_sample => 32})

    validated_format = unvalidated_format.to_validated_format
    assert_equal(:float, validated_format.sample_format)
    assert_equal(2, validated_format.channels)
    assert_equal(32, validated_format.bits_per_sample)
    assert_equal(44100, validated_format.sample_rate)
    assert_equal(352800, validated_format.byte_rate)
    assert_equal(8, validated_format.block_align)
  end

  def test_to_validated_format_unsupported
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 2,
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 176400,
                                                :block_align => 4,
                                                :bits_per_sample => 16})

    assert_raises(InvalidFormatError) { unvalidated_format.to_validated_format }
  end

  def test_to_validated_format_wave_format_extensible_pcm
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 65534,
                                                :sub_audio_format_guid => SUB_FORMAT_GUID_PCM,
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 176400,
                                                :block_align => 4,
                                                :bits_per_sample => 16,
                                                :valid_bits_per_sample => 16})

    validated_format = unvalidated_format.to_validated_format
    assert_equal(:pcm, validated_format.sample_format)
    assert_equal(2, validated_format.channels)
    assert_equal(16, validated_format.bits_per_sample)
    assert_equal(44100, validated_format.sample_rate)
    assert_equal(176400, validated_format.byte_rate)
    assert_equal(4, validated_format.block_align)
  end

  def test_to_validated_format_wave_format_extensible_float
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 65534,
                                                :sub_audio_format_guid => SUB_FORMAT_GUID_FLOAT,
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 352800,
                                                :block_align => 8,
                                                :bits_per_sample => 32,
                                                :valid_bits_per_sample => 32})

    validated_format = unvalidated_format.to_validated_format
    assert_equal(:float, validated_format.sample_format)
    assert_equal(2, validated_format.channels)
    assert_equal(32, validated_format.bits_per_sample)
    assert_equal(44100, validated_format.sample_rate)
    assert_equal(352800, validated_format.byte_rate)
    assert_equal(8, validated_format.block_align)
  end

  def test_to_validated_format_wave_format_extensible_unsupported_sub_format
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 65534,
                                                :sub_audio_format_guid => "\x02\x00\x00\x00\x00\x00\x10\x00\x80\x00\x00\xAA\x00\x38\x9B\x71",  # ADPCM
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 176400,
                                                :block_align => 4,
                                                :bits_per_sample => 16,
                                                :valid_bits_per_sample => 16})

    assert_raises(InvalidFormatError) { unvalidated_format.to_validated_format }
  end

  def test_to_validated_format_wave_format_extensible_unsupported_valid_bits_per_sample
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 65534,
                                                :sub_audio_format_guid => SUB_FORMAT_GUID_PCM,
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 176400,
                                                :block_align => 4,
                                                :bits_per_sample => 14,
                                                :valid_bits_per_sample => 14})

    assert_raises(InvalidFormatError) { unvalidated_format.to_validated_format }
  end

  def test_to_validated_format_wave_format_extensible_valid_bits_per_sample_differs_from_container_size
    unvalidated_format = UnvalidatedFormat.new({:audio_format => 65534,
                                                :sub_audio_format_guid => SUB_FORMAT_GUID_PCM,
                                                :channels => 2,
                                                :sample_rate => 44100,
                                                :byte_rate => 176400,
                                                :block_align => 4,
                                                :bits_per_sample => 16,
                                                :valid_bits_per_sample => 14})

    assert_raises(UnsupportedFormatError) { unvalidated_format.to_validated_format }
  end
end
