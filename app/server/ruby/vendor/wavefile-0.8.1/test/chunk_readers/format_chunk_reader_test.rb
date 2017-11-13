require 'minitest/autorun'
require 'wavefile.rb'

include WaveFile
include WaveFile::ChunkReaders

class FormatChunkReaderTest < Minitest::Test
  def test_basic_pcm_no_extension
    io = StringIO.new
    io.syswrite([1].pack(UNSIGNED_INT_16))   # Audio format
    io.syswrite([2].pack(UNSIGNED_INT_16))   # Channels
    io.syswrite([44100].pack(UNSIGNED_INT_32))   # Sample rate
    io.syswrite([176400].pack(UNSIGNED_INT_32))   # Byte rate
    io.syswrite([4].pack(UNSIGNED_INT_16))   # Block align
    io.syswrite([16].pack(UNSIGNED_INT_16))   # Bits per sample
    io.syswrite("data")   # Start of the next chunk
    io.rewind

    format_chunk_reader = FormatChunkReader.new(io, 16)
    unvalidated_format = format_chunk_reader.read

    assert_equal(1, unvalidated_format.audio_format)
    assert_equal(2, unvalidated_format.channels)
    assert_equal(44100, unvalidated_format.sample_rate)
    assert_equal(176400, unvalidated_format.byte_rate)
    assert_equal(4, unvalidated_format.block_align)
    assert_equal(16, unvalidated_format.bits_per_sample)

    io.close
  end

  # Test that a file with data that isn't valid configuration
  # is still read properly.
  def test_gibberish_no_extension
    io = StringIO.new
    io.syswrite([555].pack(UNSIGNED_INT_16))   # Audio format
    io.syswrite([111].pack(UNSIGNED_INT_16))   # Channels
    io.syswrite([12345].pack(UNSIGNED_INT_32))   # Sample rate
    io.syswrite([9].pack(UNSIGNED_INT_32))   # Byte rate
    io.syswrite([8000].pack(UNSIGNED_INT_16))   # Block align
    io.syswrite([23433].pack(UNSIGNED_INT_16))   # Bits per sample
    io.syswrite("data")   # Start of the next chunk
    io.rewind

    format_chunk_reader = FormatChunkReader.new(io, 16)
    unvalidated_format = format_chunk_reader.read

    assert_equal(555, unvalidated_format.audio_format)
    assert_equal(111, unvalidated_format.channels)
    assert_equal(12345, unvalidated_format.sample_rate)
    assert_equal(9, unvalidated_format.byte_rate)
    assert_equal(8000, unvalidated_format.block_align)
    assert_equal(23433, unvalidated_format.bits_per_sample)

    io.close
  end

  def test_basic_float_with_empty_extension
    io = StringIO.new
    io.syswrite([3].pack(UNSIGNED_INT_16))   # Audio format
    io.syswrite([2].pack(UNSIGNED_INT_16))   # Channels
    io.syswrite([44100].pack(UNSIGNED_INT_32))   # Sample rate
    io.syswrite([352800].pack(UNSIGNED_INT_32))   # Byte rate
    io.syswrite([8].pack(UNSIGNED_INT_16))   # Block align
    io.syswrite([32].pack(UNSIGNED_INT_16))   # Bits per sample
    io.syswrite([0].pack(UNSIGNED_INT_16))   # Extension size
    io.syswrite("data")   # Start of the next chunk
    io.rewind

    format_chunk_reader = FormatChunkReader.new(io, 18)
    unvalidated_format = format_chunk_reader.read

    assert_equal(3, unvalidated_format.audio_format)
    assert_equal(2, unvalidated_format.channels)
    assert_equal(44100, unvalidated_format.sample_rate)
    assert_equal(352800, unvalidated_format.byte_rate)
    assert_equal(8, unvalidated_format.block_align)
    assert_equal(32, unvalidated_format.bits_per_sample)

    io.close
  end

  def test_wave_format_extensible
    io = StringIO.new
    io.syswrite([65534].pack(UNSIGNED_INT_16))   # Audio format
    io.syswrite([2].pack(UNSIGNED_INT_16))   # Channels
    io.syswrite([44100].pack(UNSIGNED_INT_32))   # Sample rate
    io.syswrite([264600].pack(UNSIGNED_INT_32))   # Byte rate
    io.syswrite([6].pack(UNSIGNED_INT_16))   # Block align
    io.syswrite([24].pack(UNSIGNED_INT_16))   # Bits per sample
    io.syswrite([22].pack(UNSIGNED_INT_16))   # Extension size
    io.syswrite([20].pack(UNSIGNED_INT_16))   # Valid bits per sample
    io.syswrite([0].pack(UNSIGNED_INT_32))   # Channel mask
    io.syswrite(SUB_FORMAT_GUID_PCM)
    io.syswrite("data")   # Start of the next chunk
    io.rewind

    format_chunk_reader = FormatChunkReader.new(io, 40)
    unvalidated_format = format_chunk_reader.read

    assert_equal(65534, unvalidated_format.audio_format)
    assert_equal(2, unvalidated_format.channels)
    assert_equal(44100, unvalidated_format.sample_rate)
    assert_equal(264600, unvalidated_format.byte_rate)
    assert_equal(6, unvalidated_format.block_align)
    assert_equal(24, unvalidated_format.bits_per_sample)

    assert_equal(20, unvalidated_format.valid_bits_per_sample)
    assert_equal(SUB_FORMAT_GUID_PCM, unvalidated_format.sub_audio_format_guid)

    io.close
  end

  def test_chunk_size_too_small
    io = StringIO.new
    io.syswrite([1].pack(UNSIGNED_INT_16))   # Audio format
    io.syswrite([2].pack(UNSIGNED_INT_16))   # Channels
    io.syswrite([44100].pack(UNSIGNED_INT_32))   # Sample rate
    io.syswrite([176400].pack(UNSIGNED_INT_32))   # Byte rate
    io.syswrite([4].pack(UNSIGNED_INT_16))   # Block align
    io.syswrite([16].pack(UNSIGNED_INT_16))   # Bits per sample
    io.syswrite("data")   # Start of the next chunk
    io.rewind

    format_chunk_reader = FormatChunkReader.new(io, 15)
    assert_raises(InvalidFormatError) { format_chunk_reader.read }

    io.close
  end
end
