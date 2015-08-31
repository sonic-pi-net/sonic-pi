module WaveFile
  # Contains metadata about an existing wave file. Returned by Reader.info.
  #
  # This class is immutable - once a new Info is constructed, it can't be modified.
  class Info
    def initialize(file_name, raw_format_chunk, sample_frame_count)    # :nodoc:
      @file_name = file_name
      @audio_format = raw_format_chunk[:audio_format]
      @channels = raw_format_chunk[:channels]
      @bits_per_sample = raw_format_chunk[:bits_per_sample]
      @sample_rate = raw_format_chunk[:sample_rate]
      @byte_rate = raw_format_chunk[:byte_rate]
      @block_align = raw_format_chunk[:block_align]
      @sample_frame_count = sample_frame_count

      @duration = Duration.new(@sample_frame_count, @sample_rate)
    end

    # Returns the name of file this Info contains metadata about.
    attr_reader :file_name

    # Returns a Fixnum indicating the audio format, such as 1 for PCM or 3 for IEEE float.
    attr_reader :audio_format
    
    # Returns the number of channels, such as 1 or 2.
    attr_reader :channels
    
    # Returns the number of bits per sample, such as 8, 16, 32, or 64.
    attr_reader :bits_per_sample
    
    # Returns the number of samples per second, such as 44100.
    attr_reader :sample_rate
    
    # Returns the number of bytes contained in 1 second of sample data. 
    # Is equivalent to block_align * sample_rate.
    attr_reader :byte_rate
    
    # Returns the number of bytes in each sample frame. For example, in a 16-bit stereo file, 
    # this will be 4 (2 bytes for each 16-bit sample, times 2 channels).
    attr_reader :block_align

    # Returns the total number of sample frames in the file. A sample frame contains a single 
    # sample for each channel. So if there are 1,000 sample frames in a stereo file, this means 
    # there are 1,000 left-channel samples and 1,000 right-channel samples.
    attr_reader :sample_frame_count
    
    # Returns a Duration instance for the total number of sample frames in the file
    attr_reader :duration
  end
end
