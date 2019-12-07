require 'minitest/autorun'
require 'wavefile.rb'

include WaveFile

class BufferTest < Minitest::Test
  def test_convert
    old_format = Format.new(:mono, :pcm_16, 44100)
    new_format = Format.new(:stereo, :pcm_16, 22050)

    old_buffer = Buffer.new([-100, 0, 200], old_format)
    new_buffer = old_buffer.convert(new_format)

    assert_equal([-100, 0, 200], old_buffer.samples)
    assert_equal(1, old_buffer.channels)
    assert_equal(16, old_buffer.bits_per_sample)
    assert_equal(44100, old_buffer.sample_rate)

    assert_equal([[-100, -100], [0, 0], [200, 200]], new_buffer.samples)
    assert_equal(2, new_buffer.channels)
    assert_equal(16, new_buffer.bits_per_sample)
    assert_equal(22050, new_buffer.sample_rate)
  end

  def test_convert!
    old_format = Format.new(:mono, :pcm_16, 44100)
    new_format = Format.new(:stereo, :pcm_16, 22050)

    old_buffer = Buffer.new([-100, 0, 200], old_format)
    new_buffer = old_buffer.convert!(new_format)

    assert(old_buffer.equal?(new_buffer))
    assert_equal([[-100, -100], [0, 0], [200, 200]], old_buffer.samples)
    assert_equal(2, old_buffer.channels)
    assert_equal(16, old_buffer.bits_per_sample)
    assert_equal(22050, old_buffer.sample_rate)
  end


  def test_convert_buffer_channels
    Format::SUPPORTED_BITS_PER_SAMPLE[:pcm].each do |bits_per_sample|
      format_code = "pcm_#{bits_per_sample}".to_sym

      [44100, 22050].each do |sample_rate|
        # Assert that not changing the number of channels is a no-op
        b = Buffer.new([-100, 0, 200], Format.new(:mono, format_code, sample_rate))
        b.convert!(Format.new(:mono, format_code, sample_rate))
        assert_equal([-100, 0, 200], b.samples)

        # Mono => Stereo
        b = Buffer.new([-100, 0, 200], Format.new(:mono, format_code, sample_rate))
        b.convert!(Format.new(:stereo, format_code, sample_rate))
        assert_equal([[-100, -100], [0, 0], [200, 200]], b.samples)

        # Mono => 3-channel
        b = Buffer.new([-100, 0, 200], Format.new(:mono, format_code, sample_rate))
        b.convert!(Format.new(3, format_code, sample_rate))
        assert_equal([[-100, -100, -100], [0, 0, 0], [200, 200, 200]], b.samples)

        # Stereo => Mono
        b = Buffer.new([[-100, -100], [0, 0], [200, 50], [8, 1]], Format.new(:stereo, format_code, sample_rate))
        b.convert!(Format.new(:mono, format_code, sample_rate))
        assert_equal([-100, 0, 125, 4], b.samples)

        # 3-channel => Mono
        b = Buffer.new([[-100, -100, -100], [0, 0, 0], [200, 50, 650], [5, 1, 1], [5, 1, 2]],
                       Format.new(3, format_code, sample_rate))
        b.convert!(Format.new(:mono, format_code, sample_rate))
        assert_equal([-100, 0, 300, 2, 2], b.samples)

        # 3-channel => Stereo
        b = Buffer.new([[-100, -100, -100], [1, 2, 3], [200, 50, 650]],
                       Format.new(3, format_code, sample_rate))
        b.convert!(Format.new(:stereo, format_code, sample_rate))
        assert_equal([[-100, -100], [1, 2], [200, 50]], b.samples)

        # Unsupported conversion (4-channel => 3-channel)
        b = Buffer.new([[-100, 200, -300, 400], [1, 2, 3, 4]],
                       Format.new(4, format_code, sample_rate))
        assert_raises(BufferConversionError) { b.convert!(Format.new(3, format_code, sample_rate)) }
      end
    end
  end

  def test_convert_buffer_bits_per_sample_no_op
    Format::SUPPORTED_BITS_PER_SAMPLE[:pcm].each do |bits_per_sample|
      format_code = "pcm_#{bits_per_sample}".to_sym

      b = Buffer.new([0, 128, 255], Format.new(:mono, format_code, 44100))
      b.convert!(Format.new(:mono, format_code, 44100))

      # Target format is the same as the original format, so the sample data should not change
      assert_equal([0, 128, 255], b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_8_to_16
    # Mono
    b = Buffer.new([0, 32, 64, 96, 128, 160, 192, 223, 255], Format.new(:mono, :pcm_8, 44100))
    b.convert!(Format.new(:mono, :pcm_16, 44100))
    assert_equal([-32768, -24576, -16384, -8192, 0, 8192, 16384, 24320, 32512], b.samples)

    # Stereo
    b = Buffer.new([[0, 255],  [32, 223], [64, 192], [96, 160], [128, 128],
                    [160, 96], [192, 64], [223, 32], [255, 0]],
                   Format.new(:stereo, :pcm_8, 44100))
    b.convert!(Format.new(:stereo, :pcm_16, 44100))
    assert_equal([[-32768, 32512], [-24576, 24320], [-16384, 16384], [-8192, 8192], [0, 0],
                  [8192, -8192],   [16384, -16384], [24320, -24576], [32512, -32768]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_8_to_24
    # Mono
    b = Buffer.new([0, 32, 64, 96, 128, 160, 192, 223, 255], Format.new(:mono, :pcm_8, 44100))
    b.convert!(Format.new(:mono, :pcm_24, 44100))
    assert_equal([-8388608, -6291456, -4194304, -2097152, 0, 2097152, 4194304, 6225920, 8323072], b.samples)

    # Stereo
    b = Buffer.new([[0, 255],  [32, 223], [64, 192], [96, 160], [128, 128],
                    [160, 96], [192, 64], [223, 32], [255, 0]],
                   Format.new(:stereo, :pcm_8, 44100))
    b.convert!(Format.new(:stereo, :pcm_24, 44100))
    assert_equal([[-8388608, 8323072], [-6291456, 6225920], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                  [2097152, -2097152],   [4194304, -4194304], [6225920, -6291456], [8323072, -8388608]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_8_to_32
    # Mono
    b = Buffer.new([0, 32, 64, 96, 128, 160, 192, 223, 255], Format.new(:mono, :pcm_8, 44100))
    b.convert!(Format.new(:mono, :pcm_32, 44100))
    assert_equal([-2147483648, -1610612736, -1073741824, -536870912, 0, 536870912, 1073741824, 1593835520, 2130706432], b.samples)

    # Stereo
    b = Buffer.new([[0, 255],  [32, 223], [64, 192], [96, 160], [128, 128],
                    [160, 96], [192, 64], [223, 32], [255, 0]],
                   Format.new(:stereo, :pcm_8, 44100))
    b.convert!(Format.new(:stereo, :pcm_32, 44100))
    assert_equal([[-2147483648, 2130706432], [-1610612736, 1593835520], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                  [536870912, -536870912],   [1073741824, -1073741824], [1593835520, -1610612736], [2130706432, -2147483648]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_8_to_float
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([0, 32, 64, 96, 128, 160, 192, 223, 255], Format.new(:mono, :pcm_8, 44100))
      b.convert!(Format.new(:mono, float_format, 44100))
      assert_equal([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.7421875, 0.9921875], b.samples)

      # Stereo
      b = Buffer.new([[0, 255],  [32, 223], [64, 192], [96, 160], [128, 128],
                      [160, 96], [192, 64], [223, 32], [255, 0]],
                     Format.new(:stereo, :pcm_8, 44100))
      b.convert!(Format.new(:stereo, float_format, 44100))
      assert_equal([[-1.0, 0.9921875], [-0.75, 0.7421875], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                    [0.25, -0.25],   [0.5, -0.5], [0.7421875, -0.75], [0.9921875, -1.0]],
                   b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_16_to_8
    # Mono
    b = Buffer.new([-32768, -24576, -16384, -8192, 0, 8192, 16384, 24575, 32767], Format.new(:mono, :pcm_16, 44100))
    b.convert!(Format.new(:mono, :pcm_8, 44100))
    assert_equal([0, 32, 64, 96, 128, 160, 192, 223, 255], b.samples)

    # Stereo
    b = Buffer.new([[-32768, 32767], [-24576, 24575], [-16384, 16384], [-8192, 8192], [0, 0],
                    [8192, -8192],   [16384, -16384], [24575, -24576], [32767, -32768]],
                   Format.new(:stereo, :pcm_16, 44100))
    b.convert!(Format.new(:stereo, :pcm_8, 44100))
    assert_equal([[0, 255],  [32, 223], [64, 192], [96, 160], [128, 128],
                  [160, 96], [192, 64], [223, 32], [255, 0]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_16_to_24
    # Mono
    b = Buffer.new([-32768, -24576, -16384, -8192, 0, 8192, 16384, 24575, 32767], Format.new(:mono, :pcm_16, 44100))
    b.convert!(Format.new(:mono, :pcm_24, 44100))
    assert_equal([-8388608, -6291456, -4194304, -2097152, 0, 2097152, 4194304, 6291200, 8388352], b.samples)

    # Stereo
    b = Buffer.new([[-32768, 32767], [-24576, 24575], [-16384, 16384], [-8192, 8192], [0, 0],
                    [8192, -8192],   [16384, -16384], [24575, -24576], [32767, -32768]],
                   Format.new(:stereo, :pcm_16, 44100))
    b.convert!(Format.new(:stereo, :pcm_24, 44100))
    assert_equal([[-8388608, 8388352], [-6291456, 6291200], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                  [2097152, -2097152], [4194304, -4194304], [6291200, -6291456], [8388352, -8388608]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_16_to_32
    # Mono
    b = Buffer.new([-32768, -24576, -16384, -8192, 0, 8192, 16384, 24575, 32767], Format.new(:mono, :pcm_16, 44100))
    b.convert!(Format.new(:mono, :pcm_32, 44100))
    assert_equal([-2147483648, -1610612736, -1073741824, -536870912, 0, 536870912, 1073741824, 1610547200, 2147418112], b.samples)

    # Stereo
    b = Buffer.new([[-32768, 32767], [-24576, 24575], [-16384, 16384], [-8192, 8192], [0, 0],
                    [8192, -8192],   [16384, -16384], [24575, -24576], [32767, -32768]],
                   Format.new(:stereo, :pcm_16, 44100))
    b.convert!(Format.new(:stereo, :pcm_32, 44100))
    assert_equal([[-2147483648, 2147418112], [-1610612736, 1610547200], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                  [536870912, -536870912],   [1073741824, -1073741824], [1610547200, -1610612736], [2147418112, -2147483648]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_16_to_float
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([-32768, -24576, -16384, -8192, 0, 8192, 16384, 24575, 32767], Format.new(:mono, :pcm_16, 44100))
      b.convert!(Format.new(:mono, float_format, 44100))
      assert_equal([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.749969482421875, 0.999969482421875], b.samples)

      # Stereo
      b = Buffer.new([[-32768, 32767], [-24576, 24575], [-16384, 16384], [-8192, 8192], [0, 0],
                      [8192, -8192],   [16384, -16384], [24575, -24576], [32767, -32768]],
                     Format.new(:stereo, :pcm_16, 44100))
      b.convert!(Format.new(:stereo, float_format, 44100))
      assert_equal([[-1.0, 0.999969482421875], [-0.75, 0.749969482421875], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                    [0.25, -0.25], [0.5, -0.5], [0.749969482421875, -0.75], [0.999969482421875, -1.0]],
                   b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_24_to_8
    # Mono
    b = Buffer.new([-8388608, -6291456, -4194304, -2097152, 0, 2097152, 4194304, 6291455, 8388607],
                   Format.new(:mono, :pcm_24, 44100))
    b.convert!(Format.new(:mono, :pcm_8, 44100))
    assert_equal([0, 32, 64, 96, 128, 160, 192, 223, 255], b.samples)

    # Stereo
    b = Buffer.new([[-8388608, 8388607], [-6291456, 6291455], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                    [2097152, -2097152], [4194304, -4194304], [6291455, -6291456], [8388607, -8388608]],
                   Format.new(:stereo, :pcm_24, 44100))
    b.convert!(Format.new(:stereo, :pcm_8, 44100))
    assert_equal([[0, 255],  [32, 223], [64, 192], [96, 160], [128, 128],
                  [160, 96], [192, 64], [223, 32], [255, 0]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_24_to_16
    # Mono
    b = Buffer.new([-8388608, -6291456, -4194304, -2097152, 0, 2097152, 4194304, 6291455, 8388607],
                   Format.new(:mono, :pcm_24, 44100))
    b.convert!(Format.new(:mono, :pcm_16, 44100))
    assert_equal([-32768, -24576, -16384, -8192, 0, 8192, 16384, 24575, 32767], b.samples)

    # Stereo
    b = Buffer.new([[-8388608, 8388607], [-6291456, 6291455], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                    [2097152, -2097152], [4194304, -4194304], [6291455, -6291456], [8388607, -8388608]],
                   Format.new(:stereo, :pcm_24, 44100))
    b.convert!(Format.new(:stereo, :pcm_16, 44100))
    assert_equal([[-32768, 32767], [-24576, 24575], [-16384, 16384], [-8192, 8192], [0, 0],
                  [8192, -8192],   [16384, -16384], [24575, -24576], [32767, -32768]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_24_to_32
    # Mono
    b = Buffer.new([-8388608, -6291456, -4194304, -2097152, 0, 2097152, 4194304, 6291455, 8388607],
                   Format.new(:mono, :pcm_24, 44100))
    b.convert!(Format.new(:mono, :pcm_32, 44100))
    assert_equal([-2147483648, -1610612736, -1073741824, -536870912, 0, 536870912, 1073741824, 1610612480, 2147483392], b.samples)

    # Stereo
    b = Buffer.new([[-8388608, 8388607], [-6291456, 6291455], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                    [2097152, -2097152], [4194304, -4194304], [6291455, -6291456], [8388607, -8388608]],
                   Format.new(:stereo, :pcm_24, 44100))
    b.convert!(Format.new(:stereo, :pcm_32, 44100))
    assert_equal([[-2147483648, 2147483392], [-1610612736, 1610612480], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                  [536870912, -536870912],   [1073741824, -1073741824], [1610612480, -1610612736], [2147483392, -2147483648]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_24_to_float
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([-8388608, -6291456, -4194304, -2097152, 0, 2097152, 4194304, 6291455, 8388607],
                     Format.new(:mono, :pcm_24, 44100))
      b.convert!(Format.new(:mono, float_format, 44100))
      assert_equal([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.7499998807907104, 0.9999998807907104], b.samples)

      # Stereo
      b = Buffer.new([[-8388608, 8388607], [-6291456, 6291455], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                      [2097152, -2097152], [4194304, -4194304], [6291455, -6291456], [8388607, -8388608]],
                     Format.new(:stereo, :pcm_24, 44100))
      b.convert!(Format.new(:stereo, float_format, 44100))
      assert_equal([[-1.0, 0.9999998807907104], [-0.75, 0.7499998807907104 ], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                    [0.25, -0.25], [0.5, -0.5], [0.7499998807907104 , -0.75], [0.9999998807907104, -1.0]],
                   b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_32_to_8
    # Mono
    b = Buffer.new([-2147483648, -1610612736, -1073741824, -536870912, 0, 536870912, 1073741824, 1610612735, 2147483647],
                   Format.new(:mono, :pcm_32, 44100))
    b.convert!(Format.new(:mono, :pcm_8, 44100))
    assert_equal([0, 32, 64, 96, 128, 160, 192, 223, 255], b.samples)

    # Stereo
    b = Buffer.new([[-2147483648, 2147483647], [-1610612736, 1610612735], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                    [536870912, -536870912],   [1073741824, -1073741824], [1610612735, -1610612736], [2147483647, -2147483648]],
                   Format.new(:stereo, :pcm_32, 44100))
    b.convert!(Format.new(:stereo, :pcm_8, 44100))
    assert_equal([[0, 255],  [32, 223], [64, 192], [96, 160], [128, 128],
                  [160, 96], [192, 64], [223, 32], [255, 0]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_32_to_16
    # Mono
    b = Buffer.new([-2147483648, -1610612736, -1073741824, -536870912, 0, 536870912, 1073741824, 1610612735, 2147483647],
                   Format.new(:mono, :pcm_32, 44100))
    b.convert!(Format.new(:mono, :pcm_16, 44100))
    assert_equal([-32768, -24576, -16384, -8192, 0, 8192, 16384, 24575, 32767], b.samples)

    # Stereo
    b = Buffer.new([[-2147483648, 2147483647], [-1610612736, 1610612735], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                  [536870912, -536870912],   [1073741824, -1073741824], [1610612735, -1610612736], [2147483647, -2147483648]],
                   Format.new(:stereo, :pcm_32, 44100))
    b.convert!(Format.new(:stereo, :pcm_16, 44100))
    assert_equal([[-32768, 32767], [-24576, 24575], [-16384, 16384], [-8192, 8192], [0, 0],
                  [8192, -8192],   [16384, -16384], [24575, -24576], [32767, -32768]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_32_to_24
    # Mono
    b = Buffer.new([-2147483648, -1610612736, -1073741824, -536870912, 0, 536870912, 1073741824, 1610612735, 2147483647],
                   Format.new(:mono, :pcm_32, 44100))
    b.convert!(Format.new(:mono, :pcm_24, 44100))
    assert_equal([-8388608, -6291456, -4194304, -2097152, 0, 2097152, 4194304, 6291455, 8388607], b.samples)

    # Stereo
    b = Buffer.new([[-2147483648, 2147483647], [-1610612736, 1610612735], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                  [536870912, -536870912],   [1073741824, -1073741824], [1610612735, -1610612736], [2147483647, -2147483648]],
                   Format.new(:stereo, :pcm_32, 44100))
    b.convert!(Format.new(:stereo, :pcm_24, 44100))
    assert_equal([[-8388608, 8388607], [-6291456, 6291455], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                  [2097152, -2097152],   [4194304, -4194304], [6291455, -6291456], [8388607, -8388608]],
                 b.samples)
  end

  def test_convert_buffer_bits_per_sample_32_to_float
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([-2147483648, -1610612736, -1073741824, -536870912, 0, 536870912, 1073741824, 1610612735, 2147483647],
                     Format.new(:mono, :pcm_32, 44100))
      b.convert!(Format.new(:mono, float_format, 44100))
      assert_equal([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.7499999995343387, 0.9999999995343387], b.samples)

      # Stereo
      b = Buffer.new([[-2147483648, 2147483647], [-1610612736, 1610612735], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                      [536870912, -536870912],   [1073741824, -1073741824], [1610612735, -1610612736], [2147483647, -2147483648]],
                     Format.new(:stereo, :pcm_32, 44100))
      b.convert!(Format.new(:stereo, float_format, 44100))
      assert_equal([[-1.0, 0.9999999995343387], [-0.75, 0.7499999995343387], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                    [0.25, -0.25], [0.5, -0.5], [0.7499999995343387, -0.75], [0.9999999995343387, -1.0]],
                   b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_float_to_8
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0], Format.new(:mono, float_format, 44100))
      b.convert!(Format.new(:mono, :pcm_8, 44100))
      assert_equal([1, 33, 64, 96, 128, 160, 192, 223, 255], b.samples)

      # Stereo
      b = Buffer.new([[-1.0, 1.0], [-0.75, 0.75], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                      [0.25, -0.25],   [0.5, -0.5], [0.75, -0.75], [1.0, -1.0]],
                     Format.new(:stereo, float_format, 44100))
      b.convert!(Format.new(:stereo, :pcm_8, 44100))
      assert_equal([[1, 255], [33, 223], [64, 192], [96, 160], [128, 128],
                    [160, 96], [192, 64], [223, 33], [255, 1]],
                   b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_float_to_16
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0], Format.new(:mono, float_format, 44100))
      b.convert!(Format.new(:mono, :pcm_16, 44100))
      assert_equal([-32767, -24575, -16384, -8192, 0, 8192, 16384, 24575, 32767], b.samples)

      # Stereo
      b = Buffer.new([[-1.0, 1.0], [-0.75, 0.75], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                      [0.25, -0.25], [0.5, -0.5], [0.75, -0.75], [1.0, -1.0]],
                     Format.new(:stereo, float_format, 44100))
      b.convert!(Format.new(:stereo, :pcm_16, 44100))
      assert_equal([[-32767, 32767], [-24575, 24575], [-16384, 16384], [-8192, 8192], [0, 0],
                    [8192, -8192], [16384, -16384], [24575, -24575], [32767, -32767]],
                   b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_float_to_24
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0], Format.new(:mono, float_format, 44100))
      b.convert!(Format.new(:mono, :pcm_24, 44100))
      assert_equal([-8388607, -6291455, -4194304, -2097152, 0, 2097152, 4194304, 6291455, 8388607], b.samples)

      # Stereo
      b = Buffer.new([[-1.0, 1.0], [-0.75, 0.75], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                      [0.25, -0.25], [0.5, -0.5], [0.75, -0.75], [1.0, -1.0]],
                     Format.new(:stereo, float_format, 44100))
      b.convert!(Format.new(:stereo, :pcm_24, 44100))
      assert_equal([[-8388607, 8388607], [-6291455, 6291455], [-4194304, 4194304], [-2097152, 2097152], [0, 0],
                    [2097152, -2097152], [4194304, -4194304], [6291455, -6291455], [8388607, -8388607]],
                   b.samples)
    end
  end

  def test_convert_buffer_bits_per_sample_float_to_32
    Format::SUPPORTED_BITS_PER_SAMPLE[:float].each do |bits_per_sample|
      float_format = "float_#{bits_per_sample}".to_sym

      # Mono
      b = Buffer.new([-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0], Format.new(:mono, float_format, 44100))
      b.convert!(Format.new(:mono, :pcm_32, 44100))
      assert_equal([-2147483647, -1610612735, -1073741824, -536870912, 0, 536870912, 1073741824, 1610612735, 2147483647], b.samples)

      # Stereo
      b = Buffer.new([[-1.0, 1.0], [-0.75, 0.75], [-0.5, 0.5], [-0.25, 0.25], [0.0, 0.0],
                      [0.25, -0.25], [0.5, -0.5], [0.75, -0.75], [1.0, -1.0]],
                     Format.new(:stereo, float_format, 44100))
      b.convert!(Format.new(:stereo, :pcm_32, 44100))
      assert_equal([[-2147483647, 2147483647], [-1610612735, 1610612735], [-1073741824, 1073741824], [-536870912, 536870912], [0, 0],
                    [536870912, -536870912], [1073741824, -1073741824], [1610612735, -1610612735], [2147483647, -2147483647]],
                   b.samples)
    end
  end
end
