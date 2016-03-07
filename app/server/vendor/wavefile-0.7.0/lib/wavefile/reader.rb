module WaveFile
  # Error that is raised when a file is not in a format supported by this Gem,
  # because it's a valid Wave file whose format is not supported by this Gem,
  # because it's a not a valid Wave file period, etc.
  class FormatError < StandardError; end

  # Error that is raised when trying to read from a file that is either not a wave file, 
  # or that is not valid according to the wave file spec.
  class InvalidFormatError < FormatError; end

  # Error that is raised when trying to read from a valid wave file that has its sample data 
  # stored in a format that Reader doesn't understand.
  class UnsupportedFormatError < FormatError; end


  # Provides the ability to read sample data out of a wave file, as well as query a 
  # wave file about its metadata (e.g. number of channels, sample rate, etc).
  #
  # When constructing a Reader a block can be given. All data should be read inside this 
  # block, and when the block exits the Reader will automatically be closed.
  #
  #     Reader.new("my_file.wav") do |reader|
  #       # Read sample data here
  #     end
  #
  # Alternately, if a block isn't given you should make sure to call close when finished reading.
  #
  #     reader = Reader.new("my_file.wav")
  #     # Read sample data here
  #     reader.close
  class Reader
    # Returns a Reader object that is ready to start reading the specified file's sample data. 
    #
    # file_name - The name of the wave file to read from.
    # format - The format that read sample data should be returned in 
    #          (default: the wave file's internal format).
    #
    # Returns a Reader object that is ready to start reading the specified file's sample data.
    # Raises Errno::ENOENT if the specified file can't be found
    # Raises InvalidFormatError if the specified file isn't a valid wave file
    def initialize(file_name, format=nil)
      @file_name = file_name
      @file = File.open(file_name, "rb")

      begin
        @riff_reader = ChunkReaders::RiffReader.new(@file, @file_name)
      rescue InvalidFormatError
        raise InvalidFormatError, "'#{@file_name}' does not appear to be a valid Wave file"
      end
      
      @raw_native_format = @riff_reader.native_format
      @total_sample_frames = @riff_reader.data_chunk_reader.sample_frame_count
      @current_sample_frame = 0

      native_sample_format = "#{FORMAT_CODES.invert[native_format.audio_format]}_#{native_format.bits_per_sample}".to_sym

      @readable_format = true
      begin
        @native_format = Format.new(@raw_native_format.channels,
                                    native_sample_format,
                                    @raw_native_format.sample_rate)
        @pack_code = PACK_CODES[@native_format.sample_format][@native_format.bits_per_sample]
      rescue FormatError
        @readable_format = false
        @pack_code = nil
      end

      @format = (format == nil) ? (@native_format || @raw_native_format) : format

      if block_given?
        begin
          yield(self)
        ensure
          close
        end
      end
    end


    # Reads sample data of the into successive Buffers of the specified size, until there is no more 
    # sample data to be read. When all sample data has been read, the Reader is automatically closed. 
    # Each Buffer is passed to the given block.
    #
    # Note that sample_frame_count indicates the number of sample frames to read, not number of samples. 
    # A sample frame include one sample for each channel. For example, if sample_frame_count is 1024, then 
    # for a stereo file 1024 samples will be read from the left channel, and 1024 samples will be read from 
    # the right channel.
    #
    # sample_frame_count - The number of sample frames to read into each Buffer from each channel. The number 
    #                      of sample frames read into the final Buffer could be less than this size, if there 
    #                      are not enough remaining.
    #
    # Returns nothing.
    def each_buffer(sample_frame_count)
      begin
        while true do
          yield(read(sample_frame_count))
        end
      rescue EOFError
        close
      end
    end


    # Reads the specified number of sample frames from the wave file into a Buffer. Note that the Buffer will have 
    # at most sample_frame_count sample frames, but could have less if the file doesn't have enough remaining.
    #
    # sample_frame_count - The number of sample frames to read. Note that each sample frame includes a sample for 
    #                      each channel.
    #
    # Returns a Buffer containing sample_frame_count sample frames
    # Raises UnsupportedFormatError if file is in a format that can't be read by this gem
    # Raises EOFError if no samples could be read due to reaching the end of the file
    def read(sample_frame_count)
      raise UnsupportedFormatError unless @readable_format

      if @current_sample_frame >= @total_sample_frames
        #FIXME: Do something different here, because the end of the file has not actually necessarily been reached
        raise EOFError
      elsif sample_frame_count > sample_frames_remaining
        sample_frame_count = sample_frames_remaining
      end

      samples = @file.sysread(sample_frame_count * @native_format.block_align).unpack(@pack_code)
      @current_sample_frame += sample_frame_count

      if @native_format.bits_per_sample == 24
        # Since the sample data is little endian, the 3 bytes will go from least->most significant
        samples = samples.each_slice(3).map {|least_significant_byte, middle_byte, most_significant_byte|
          # Convert the byte read as "C" to one read as "c"
          most_significant_byte = [most_significant_byte].pack("c").unpack("c").first
          
          (most_significant_byte << 16) | (middle_byte << 8) | least_significant_byte
        }
      end

      if @native_format.channels > 1
        samples = samples.each_slice(@native_format.channels).to_a
      end

      buffer = Buffer.new(samples, @native_format)
      buffer.convert(@format)
    end


    # Returns true if the Reader is closed, and false if it is open and available for reading.
    def closed?
      @file.closed?
    end


    # Closes the Reader. After a Reader is closed, no more sample data can be read from it.
    #
    # Returns nothing.
    # Raises IOError if the Reader is already closed.
    def close
      @file.close
    end

    # Returns a Duration instance for the total number of sample frames in the file
    def total_duration
      Duration.new(total_sample_frames, @format.sample_rate)
    end

    # Returns a Format object describing the sample format of the Wave file being read.
    # This is not necessarily the format that the sample data will be read as - to determine
    # that, use #format.
    def native_format
      @raw_native_format
    end

    # Returns true if this is a valid Wave file and contains sample data that is in a format
    # that this class can read, and returns false if this is a valid Wave file but does not
    # contain a sample format supported by this class.
    def readable_format?
      @readable_format
    end

    # Returns the name of the Wave file that is being read
    attr_reader :file_name

    # Returns a Format object describing how sample data is being read from the Wave file (number of 
    # channels, sample format and bits per sample, etc). Note that this might be different from the 
    # underlying format of the Wave file on disk.
    attr_reader :format

    # Returns the index of the sample frame which is "cued up" for reading. I.e., the index 
    # of the next sample frame that will be read. A sample frame contains a single sample 
    # for each channel. So if there are 1,000 sample frames in a stereo file, this means 
    # there are 1,000 left-channel samples and 1,000 right-channel samples.
    attr_reader :current_sample_frame

    # Returns the total number of sample frames in the file. A sample frame contains a single 
    # sample for each channel. So if there are 1,000 sample frames in a stereo file, this means 
    # there are 1,000 left-channel samples and 1,000 right-channel samples.
    attr_reader :total_sample_frames

  private

    # The number of sample frames in the file after the current sample frame
    def sample_frames_remaining
      @total_sample_frames - @current_sample_frame
    end
  end
end
