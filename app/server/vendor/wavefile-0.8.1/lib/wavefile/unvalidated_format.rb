module WaveFile
  # Represents information about the data format for a Wave file, such as number of
  # channels, bits per sample, sample rate, and so forth. A Format instance is used
  # by Reader to indicate what format to read samples out as, and by Writer to
  # indicate what format to write samples as.
  #
  # This class is immutable - once a new Format is constructed, it can't be modified.
  class UnvalidatedFormat < Format    # :nodoc:
    # Constructs a new immutable UnvalidatedFormat.
    def initialize(fields)
      @audio_format = fields[:audio_format]
      @sub_audio_format_guid = fields[:sub_audio_format_guid]
      @channels = fields[:channels]
      @sample_rate = fields[:sample_rate]
      @byte_rate = fields[:byte_rate]
      @block_align = fields[:block_align]
      @bits_per_sample = fields[:bits_per_sample]
      @valid_bits_per_sample = fields[:valid_bits_per_sample]
    end

    attr_reader :audio_format, :sub_audio_format_guid, :valid_bits_per_sample

    def to_validated_format
      if @sub_audio_format_guid.nil?
        audio_format_code = @audio_format
      else
        if @sub_audio_format_guid == SUB_FORMAT_GUID_PCM
          audio_format_code = 1
        elsif @sub_audio_format_guid == SUB_FORMAT_GUID_FLOAT
          audio_format_code = 3
        else
          audio_format_code = nil
        end
      end

      if @valid_bits_per_sample
        if @valid_bits_per_sample != @bits_per_sample
          raise UnsupportedFormatError,
                "Sample container size (#{@bits_per_sample}) and valid bits per sample (#{@valid_bits_per_sample}) " +
                "differ."
        end

        bits_per_sample = @valid_bits_per_sample
      else
        bits_per_sample = @bits_per_sample
      end

      sample_format = "#{FORMAT_CODES.invert[audio_format_code]}_#{bits_per_sample}".to_sym

      Format.new(@channels, sample_format, @sample_rate)
    end
  end
end
