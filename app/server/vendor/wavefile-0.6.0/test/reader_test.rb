require 'test/unit'
require 'wavefile.rb'
require 'wavefile_io_test_helper.rb'

include WaveFile

class ReaderTest < Test::Unit::TestCase
  include WaveFileIOTestHelper

  FIXTURE_ROOT_PATH = "test/fixtures"


  def test_nonexistent_file
    assert_raise(Errno::ENOENT) { Reader.new(fixture("i_do_not_exist.wav")) }

    assert_raise(Errno::ENOENT) { Reader.info(fixture("i_do_not_exist.wav")) }
  end

  def test_invalid_formats
    invalid_fixtures = [
      # File contains 0 bytes
      "invalid/empty.wav",

      # File consists of "RIFF" and nothing else
      "invalid/incomplete_riff_header.wav",

      # First 4 bytes in the file are not "RIFF"
      "invalid/bad_riff_header.wav",

      # The format code in the RIFF header is not "WAVE"
      "invalid/bad_wavefile_format.wav",

      # The file consists of just a valid RIFF header
      "invalid/no_format_chunk.wav",

      # The format chunk has 0 bytes in it (despite the chunk size being 16)
      "invalid/empty_format_chunk.wav",

      # The format chunk has some data, but not all of the minimum required.
      "invalid/insufficient_format_chunk.wav",

      # The RIFF header and format chunk are OK, but there is no data chunk
      "invalid/no_data_chunk.wav",
    ]

    # Reader.new and Reader.info should raise the same errors for invalid files,
    # so run the tests for both methods.
    [:new, :info].each do |method_name|
      invalid_fixtures.each do |fixture_name|
        assert_raise(InvalidFormatError) { Reader.send(method_name, fixture(fixture_name)) }
      end
    end
  end

  def test_unsupported_formats
    unsupported_fixtures = [
      # Audio format is 2, which is not supported
      "unsupported/unsupported_audio_format.wav",

      # Bits per sample is 20, which is not supported
      "unsupported/unsupported_bits_per_sample.wav",

      # Channel count is 0
      "unsupported/bad_channel_count.wav",

      # Sample rate is 0
      "unsupported/bad_sample_rate.wav",
    ]

    unsupported_fixtures.each do |fixture_name|
      assert_raise(UnsupportedFormatError) { Reader.new(fixture(fixture_name)) }
    end
  end

  def test_initialize
    format = Format.new(:stereo, :pcm_16, 22050)

    exhaustively_test do |channels, sample_format|
      file_name = fixture("valid/valid_#{channels}_#{sample_format}_44100.wav")

      # Read native format
      reader = Reader.new(file_name)
      assert_equal(CHANNEL_ALIAS[channels], reader.format.channels)
      assert_equal(extract_bits_per_sample(sample_format), reader.format.bits_per_sample)
      assert_equal(44100, reader.format.sample_rate)
      assert_equal(false, reader.closed?)
      assert_equal(file_name, reader.file_name)
      assert_equal(0, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
      reader.close

      # Read a non-native format
      reader = Reader.new(file_name, format)
      assert_equal(2, reader.format.channels)
      assert_equal(16, reader.format.bits_per_sample)
      assert_equal(22050, reader.format.sample_rate)
      assert_equal(false, reader.closed?)
      assert_equal(file_name, reader.file_name)
      assert_equal(0, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
      reader.close

      # Block is given.
      reader = Reader.new(file_name) {|reader| reader.read(1024) }
      assert_equal(CHANNEL_ALIAS[channels], reader.format.channels)
      assert_equal(extract_bits_per_sample(sample_format), reader.format.bits_per_sample)
      assert_equal(44100, reader.format.sample_rate)
      assert(reader.closed?)
      assert_equal(file_name, reader.file_name)
      assert_equal(1024, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
    end
  end

  def test_read_native_format
    exhaustively_test do |channels, sample_format|
      buffers = read_file("valid/valid_#{channels}_#{sample_format}_44100.wav", 1024)

      assert_equal(3, buffers.length)
      assert_equal([1024, 1024, 192], buffers.map {|buffer| buffer.samples.length })
      assert_equal(SQUARE_WAVE_CYCLE[channels][sample_format] * 128, buffers[0].samples)
      assert_equal(SQUARE_WAVE_CYCLE[channels][sample_format] * 128, buffers[1].samples)
      assert_equal(SQUARE_WAVE_CYCLE[channels][sample_format] * 24,  buffers[2].samples)
    end
  end

  def test_read_with_format_conversion
    buffers = read_file("valid/valid_mono_pcm_16_44100.wav", 1024, Format.new(:stereo, :pcm_8, 22100))

    assert_equal(3, buffers.length)
    assert_equal([1024, 1024, 192], buffers.map {|buffer| buffer.samples.length })
    assert_equal(SQUARE_WAVE_CYCLE[:stereo][:pcm_8] * 128, buffers[0].samples)
    assert_equal(SQUARE_WAVE_CYCLE[:stereo][:pcm_8] * 128, buffers[1].samples)
    assert_equal(SQUARE_WAVE_CYCLE[:stereo][:pcm_8] * 24,  buffers[2].samples)
  end

  def test_read_with_padding_byte
    buffers = read_file("valid/valid_mono_pcm_8_44100_with_padding_byte.wav", 1024)

    assert_equal(3, buffers.length)
    assert_equal([1024, 1024, 191], buffers.map {|buffer| buffer.samples.length })
    assert_equal(SQUARE_WAVE_CYCLE[:mono][:pcm_8] * 128, buffers[0].samples)
    assert_equal(SQUARE_WAVE_CYCLE[:mono][:pcm_8] * 128, buffers[1].samples)
    assert_equal((SQUARE_WAVE_CYCLE[:mono][:pcm_8] * 23) + [88, 88, 88, 88, 167, 167, 167],
                 buffers[2].samples)
  end

  def test_each_buffer_no_block_given
    reader = Reader.new(fixture("valid/valid_mono_pcm_16_44100.wav"))
    assert_raise(LocalJumpError) { reader.each_buffer(1024) }
  end

  def test_each_buffer_native_format
    exhaustively_test do |channels, sample_format|
      reader = Reader.new(fixture("valid/valid_#{channels}_#{sample_format}_44100.wav"))

      buffers = []
      reader.each_buffer(1024) {|buffer| buffers << buffer }

      assert(reader.closed?)
      assert_equal(3, buffers.length)
      assert_equal([1024, 1024, 192], buffers.map {|buffer| buffer.samples.length })
      assert_equal(SQUARE_WAVE_CYCLE[channels][sample_format] * 128, buffers[0].samples)
      assert_equal(SQUARE_WAVE_CYCLE[channels][sample_format] * 128, buffers[1].samples)
      assert_equal(SQUARE_WAVE_CYCLE[channels][sample_format] * 24,  buffers[2].samples)
      assert_equal(2240, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
    end
  end

  def test_each_buffer_with_format_conversion
    reader = Reader.new(fixture("valid/valid_mono_pcm_16_44100.wav"), Format.new(:stereo, :pcm_8, 22050))
    assert_equal(2, reader.format.channels)
    assert_equal(8, reader.format.bits_per_sample)
    assert_equal(22050, reader.format.sample_rate)

    buffers = []
    reader.each_buffer(1024) {|buffer| buffers << buffer }

    assert_equal(3, buffers.length)
    assert_equal([1024, 1024, 192], buffers.map {|buffer| buffer.samples.length })
    assert_equal(SQUARE_WAVE_CYCLE[:stereo][:pcm_8] * 128, buffers[0].samples)
    assert_equal(SQUARE_WAVE_CYCLE[:stereo][:pcm_8] * 128, buffers[1].samples)
    assert_equal(SQUARE_WAVE_CYCLE[:stereo][:pcm_8] * 24,  buffers[2].samples)
    assert_equal(2240, reader.current_sample_frame)
    assert_equal(2240, reader.total_sample_frames)
  end

  def test_each_buffer_with_padding_byte
    buffers = []
    reader = Reader.new(fixture("valid/valid_mono_pcm_8_44100_with_padding_byte.wav"))
    reader.each_buffer(1024) {|buffer| buffers << buffer }

    assert_equal(3, buffers.length)
    assert_equal([1024, 1024, 191], buffers.map {|buffer| buffer.samples.length })
    assert_equal(SQUARE_WAVE_CYCLE[:mono][:pcm_8] * 128, buffers[0].samples)
    assert_equal(SQUARE_WAVE_CYCLE[:mono][:pcm_8] * 128, buffers[1].samples)
    assert_equal((SQUARE_WAVE_CYCLE[:mono][:pcm_8] * 23) + [88, 88, 88, 88, 167, 167, 167],
                 buffers[2].samples)
    assert_equal(2239, reader.current_sample_frame)
    assert_equal(2239, reader.total_sample_frames)
  end

  def test_read_non_data_chunk_with_padding_byte
    # This fixture file contains a JUNK chunk with an odd size, aligned to an even number of
    # bytes via an appended padding byte. If the padding byte is not taken into account, this
    # test will blow up due to the file not being synced up to the data chunk in the right place.
    reader = Reader.new(fixture("valid/valid_mono_pcm_16_44100_junk_chunk_with_padding_byte.wav"))
    buffer = reader.read(1024)
    assert_equal(buffer.samples, SQUARE_WAVE_CYCLE[:mono][:pcm_16] * 128)
    assert_equal(1024, reader.current_sample_frame)
    assert_equal(2240, reader.total_sample_frames)
  end

  def test_closed?
    reader = Reader.new(fixture("valid/valid_mono_pcm_16_44100.wav"))
    assert_equal(false, reader.closed?)
    reader.close
    assert(reader.closed?)

    # For Reader.each_buffer
    reader = Reader.new(fixture("valid/valid_mono_pcm_16_44100.wav"))
    assert_equal(false, reader.closed?)
    reader.each_buffer(1024) do |buffer|
      # No-op
    end
    assert_equal(true, reader.closed?)
  end

  def test_read_after_close
    reader = Reader.new(fixture("valid/valid_mono_pcm_16_44100.wav"))
    buffer = reader.read(1024)
    reader.close
    assert_raise(IOError) { reader.read(1024) }
  end

  def test_sample_counts_manual_reads
    exhaustively_test do |channels, sample_format|
      reader = Reader.new(fixture("valid/valid_#{channels}_#{sample_format}_44100.wav"))
      
      assert_equal(0, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
      test_duration({:hours => 0, :minutes => 0, :seconds => 0, :milliseconds => 50, :sample_count => 2240},
                    reader.total_duration)


      reader.read(1024)
      assert_equal(1024, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
      test_duration({:hours => 0, :minutes => 0, :seconds => 0, :milliseconds => 50, :sample_count => 2240},
                    reader.total_duration)


      reader.read(1024)
      assert_equal(2048, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
      test_duration({:hours => 0, :minutes => 0, :seconds => 0, :milliseconds => 50, :sample_count => 2240},
                    reader.total_duration)


      reader.read(192)
      assert_equal(2240, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
      test_duration({:hours => 0, :minutes => 0, :seconds => 0, :milliseconds => 50, :sample_count => 2240},
                    reader.total_duration)
 

      reader.close
      assert_equal(2240, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
      test_duration({:hours => 0, :minutes => 0, :seconds => 0, :milliseconds => 50, :sample_count => 2240},
                    reader.total_duration)
    end
  end

  def test_sample_counts_each_buffer
    exhaustively_test do |channels, sample_format|
      expected_results = [ 1024, 2048, 2240 ]

      file_name = fixture("valid/valid_#{channels}_#{sample_format}_44100.wav")
      reader = Reader.new(file_name)

      assert_equal(0, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)

      reader.each_buffer(1024) do |buffer|
        expected_result = expected_results.slice!(0)

        assert_equal(expected_result, reader.current_sample_frame)
        assert_equal(2240, reader.total_sample_frames)
      end
      
      assert_equal(2240, reader.current_sample_frame)
      assert_equal(2240, reader.total_sample_frames)
    end
  end

private

  def read_file(file_name, buffer_size, format=nil)
    buffers = []
    reader = Reader.new(fixture(file_name), format)

    begin
      while true do
        buffers << reader.read(buffer_size)
      end
    rescue EOFError
      reader.close
    end

    return buffers
  end

  def fixture(fixture_name)
    return "#{FIXTURE_ROOT_PATH}/#{fixture_name}"
  end

  def extract_bits_per_sample(sample_format)
    sample_format.to_s.split("_").last.to_i
  end

  def test_duration(expected_hash, duration)
    assert_equal(expected_hash[:hours], duration.hours)
    assert_equal(expected_hash[:minutes], duration.minutes)
    assert_equal(expected_hash[:seconds], duration.seconds)
    assert_equal(expected_hash[:milliseconds], duration.milliseconds)
    assert_equal(expected_hash[:sample_count], duration.sample_frame_count)
  end
end
