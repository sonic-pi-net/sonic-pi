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
      @channels = fields[:channels]
      @sample_rate = fields[:sample_rate]
      @byte_rate = fields[:byte_rate]
      @block_align = fields[:block_align]
      @bits_per_sample = fields[:bits_per_sample]
    end

    # Returns a number indicating the sample format, such as 1 (PCM) or 3 (Float)
    attr_reader :audio_format
  end
end

