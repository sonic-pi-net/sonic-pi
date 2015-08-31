module WaveFile
  class InvalidFormatError < StandardError; end

  # Represents information about the data format for a Wave file, such as number of 
  # channels, bits per sample, sample rate, and so forth. A Format instance is used 
  # by Reader to indicate what format to read samples out as, and by Writer to 
  # indicate what format to write samples as.
  #
  # This class is immutable - once a new Format is constructed, it can't be modified.
  class Format
    # Not using ranges because of 1.8.7 performance problems with Range.max
    MIN_CHANNELS = 1    # :nodoc:
    MAX_CHANNELS = 65535    # :nodoc:

    MIN_SAMPLE_RATE = 1    # :nodoc:
    MAX_SAMPLE_RATE = 4_294_967_296    # :nodoc:

    SUPPORTED_SAMPLE_FORMATS = [:pcm, :float]    # :nodoc:
    SUPPORTED_BITS_PER_SAMPLE = {
                                  :pcm => [8, 16, 24, 32],
                                  :float => [32, 64],
                                }    # :nodoc:

    # Constructs a new immutable Format.
    #
    # channels - The number of channels in the format. Can either be a Fixnum 
    #            (e.g. 1, 2, 3) or the symbols :mono (equivalent to 1) or 
    #            :stereo (equivalent to 2).
    # format_code - A symbol indicating the format of each sample. Consists of 
    #               two parts: a format code, and the bits per sample. The valid 
    #               values are :pcm_8, :pcm_16, :pcm_32, :float_32, :float_64,
    #               and :float (equivalent to :float_32)
    # sample_rate - The number of samples per second, such as 44100
    #
    # Examples
    #
    #   format = Format.new(1, :pcm_16, 44100)
    #   format = Format.new(:mono, :pcm_16, 44100)  # Equivalent to above
    #
    #   format = Format.new(:stereo, :float_32, 44100)
    #   format = Format.new(:stereo, :float, 44100)
    def initialize(channels, format_code, sample_rate)
      channels = normalize_channels(channels)
      sample_format, bits_per_sample = normalize_format_code(format_code)
      validate_channels(channels)
      validate_sample_format(sample_format)
      validate_bits_per_sample(sample_format, bits_per_sample)
      validate_sample_rate(sample_rate)

      @channels = channels
      @sample_format = sample_format
      @bits_per_sample = bits_per_sample
      @sample_rate = sample_rate
      @block_align = (@bits_per_sample / 8) * @channels
      @byte_rate = @block_align * @sample_rate
    end

    # Returns true if the format has 1 channel, false otherwise.
    def mono?
      @channels == 1
    end

    # Returns true if the format has 2 channels, false otherwise.
    def stereo?
      @channels == 2
    end

    # Returns the number of channels, such as 1 or 2. This will always return a 
    # Fixnum, even if the number of channels is specified with a symbol (e.g. :mono) 
    # in the constructor.
    attr_reader :channels

    # Returns a symbol indicating the sample format, such as :pcm or :float
    attr_reader :sample_format

    # Returns the number of bits per sample, such as 8, 16, 24, 32, or 64.
    attr_reader :bits_per_sample

    # Returns the number of samples per second, such as 44100.
    attr_reader :sample_rate

    # Returns the number of bytes in each sample frame. For example, in a 16-bit stereo file, 
    # this will be 4 (2 bytes for each 16-bit sample, times 2 channels).
    attr_reader :block_align

    # Returns the number of bytes contained in 1 second of sample data. 
    # Is equivalent to block_align * sample_rate.
    attr_reader :byte_rate

  private

    def normalize_channels(channels)
      if channels == :mono
        return 1
      elsif channels == :stereo
        return 2
      else
        return channels
      end
    end

    def normalize_format_code(format_code)
      if SUPPORTED_BITS_PER_SAMPLE[:pcm].include? format_code
        [:pcm, format_code]
      elsif format_code == :float
        [:float, 32]
      else
        sample_format, bits_per_sample = format_code.to_s.split("_")
        [sample_format.to_sym, bits_per_sample.to_i]
      end
    end

    def validate_sample_format(candidate_sample_format)
      unless SUPPORTED_SAMPLE_FORMATS.include? candidate_sample_format
        raise InvalidFormatError,
              "Sample format of #{candidate_sample_format} is unsupported. " +
              "Only #{SUPPORTED_SAMPLE_FORMATS.inspect} are supported."
      end
    end

    def validate_channels(candidate_channels)
      unless (MIN_CHANNELS..MAX_CHANNELS) === candidate_channels
        raise InvalidFormatError, "Invalid number of channels. Must be between 1 and #{MAX_CHANNELS}."
      end
    end

    def validate_bits_per_sample(candidate_sample_format, candidate_bits_per_sample)
      unless SUPPORTED_BITS_PER_SAMPLE[candidate_sample_format].include? candidate_bits_per_sample
        raise InvalidFormatError,
              "Bits per sample of #{candidate_bits_per_sample} is unsupported for " +
              "sample format #{candidate_sample_format}."
      end
    end

    def validate_sample_rate(candidate_sample_rate)
      unless (MIN_SAMPLE_RATE..MAX_SAMPLE_RATE) === candidate_sample_rate
        raise InvalidFormatError, "Invalid sample rate. Must be between 1 and #{MAX_SAMPLE_RATE}"
      end
    end
  end
end
