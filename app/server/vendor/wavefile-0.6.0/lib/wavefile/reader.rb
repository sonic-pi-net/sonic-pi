module WaveFile
  # Error that is raised when trying to read from a file that is either not a wave file, 
  # or that is not valid according to the wave file spec.
  class InvalidFormatError < StandardError; end

  # Error that is raised when trying to read from a valid wave file that has its sample data 
  # stored in a format that Reader doesn't understand.
  class UnsupportedFormatError < StandardError; end


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
    # Raises UnsupportedFormatError if the specified file has its sample data stored in a format
    #                               that Reader doesn't know how to process.
    def initialize(file_name, format=nil)
      @file_name = file_name
      @file = File.open(file_name, "rb")

      raw_format_chunk, sample_frame_count = HeaderReader.new(@file, @file_name).read_until_data_chunk
      @current_sample_frame = 0
      @total_sample_frames = sample_frame_count

      # Make file is in a format we can actually read
      validate_format_chunk(raw_format_chunk)

      native_sample_format = "#{FORMAT_CODES.invert[raw_format_chunk[:audio_format]]}_#{raw_format_chunk[:bits_per_sample]}".to_sym
      @native_format = Format.new(raw_format_chunk[:channels],
                                  native_sample_format,
                                  raw_format_chunk[:sample_rate])
      @pack_code = PACK_CODES[@native_format.sample_format][@native_format.bits_per_sample]
      @format = (format == nil) ? @native_format : format

      if block_given?
        begin
          yield(self)
        ensure
          close
        end
      end
    end


    # Reads metadata from the specified wave file and returns an Info object with the results. 
    # Metadata includes things like the number of channels, bits per sample, number of sample 
    # frames, sample encoding format (i.e. PCM, IEEE float, uLaw etc). See the Info object for 
    # more detail on exactly what metadata is available.
    #
    # file_name - The name of the wave file to read from
    #
    # Examples:
    #
    #   info = Reader.info("my_docs/my_sounds/my_file.wav")
    #
    # Returns an Info object containing metadata about the wave file.
    # Raises Errno::ENOENT if the specified file can't be found
    # Raises InvalidFormatError if the specified file isn't a valid wave file, or is in a format
    #                           that WaveFile can't read.
    def self.info(file_name)
      file = File.open(file_name, "rb")
      raw_format_chunk, sample_frame_count = HeaderReader.new(file, file_name).read_until_data_chunk
      file.close

      Info.new(file_name, raw_format_chunk, sample_frame_count)
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
    # Raises EOFError if no samples could be read due to reaching the end of the file
    def read(sample_frame_count)
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

    def validate_format_chunk(raw_format_chunk)
      # :byte_rate and :block_align are not checked to make sure that match :channels/:sample_rate/bits_per_sample
      # because this library doesn't use them.

      unless FORMAT_CODES.values.include? raw_format_chunk[:audio_format]
        raise UnsupportedFormatError, "Audio format is #{raw_format_chunk[:audio_format]}, " +
                                      "but only format code 1 (PCM) or 3 (floating point) is supported."
      end

      unless Format::SUPPORTED_BITS_PER_SAMPLE[FORMAT_CODES.invert[raw_format_chunk[:audio_format]]].include?(raw_format_chunk[:bits_per_sample])
        raise UnsupportedFormatError, "Bits per sample is #{raw_format_chunk[:bits_per_sample]}, " +
                                      "but only #{Format::SUPPORTED_BITS_PER_SAMPLE[:pcm].inspect} are supported."
      end

      unless raw_format_chunk[:channels] > 0
        raise UnsupportedFormatError, "Number of channels is #{raw_format_chunk[:channels]}, " +
                                      "but only #{Format::MIN_CHANNELS}-#{Format::MAX_CHANNELS} are supported."
      end

      unless raw_format_chunk[:sample_rate] > 0
        raise UnsupportedFormatError, "Sample rate is #{raw_format_chunk[:sample_rate]}, " +
                                      "but only #{Format::MIN_SAMPLE_RATE}-#{Format::MAX_SAMPLE_RATE} are supported."
      end
    end
  end


  # Used to read the RIFF chunks in a wave file up until the data chunk. Thus is can be used
  # to open a wave file and "queue it up" to the start of the actual sample data, as well as
  # extract information out of pre-data chunks, such as the format chunk.
  class HeaderReader    # :nodoc:
    RIFF_CHUNK_HEADER_SIZE = 12
    FORMAT_CHUNK_MINIMUM_SIZE = 16

    def initialize(file, file_name)
      @file = file
      @file_name = file_name
    end

    def read_until_data_chunk
      read_riff_chunk

      begin
        chunk_id = @file.sysread(4)
        chunk_size = @file.sysread(4).unpack(UNSIGNED_INT_32).first
        while chunk_id != CHUNK_IDS[:data]
          if chunk_id == CHUNK_IDS[:format]
            format_chunk = read_format_chunk(chunk_id, chunk_size)
          else
            # The RIFF specification requires that each chunk be aligned to an even number of bytes,
            # even if the byte count is an odd number.
            #
            # See http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/riffmci.pdf, page 11.
            if chunk_size.odd?
              chunk_size += 1
            end

            # Other chunk types besides the format chunk are ignored. This may change in the future.
            @file.sysread(chunk_size)
          end

          chunk_id = @file.sysread(4)
          chunk_size = @file.sysread(4).unpack(UNSIGNED_INT_32).first
        end
      rescue EOFError
        raise_error InvalidFormatError, "It doesn't have a data chunk."
      end

      if format_chunk == nil
        raise_error InvalidFormatError, "The format chunk is either missing, or it comes after the data chunk."
      end

      sample_frame_count = chunk_size / format_chunk[:block_align]

      return format_chunk, sample_frame_count
    end

  private

    def read_riff_chunk
      riff_header = {}
      riff_header[:chunk_id],
      riff_header[:chunk_size],
      riff_header[:riff_format] = read_chunk_body(CHUNK_IDS[:riff], RIFF_CHUNK_HEADER_SIZE).unpack("a4Va4")

      unless riff_header[:chunk_id] == CHUNK_IDS[:riff]
        raise_error InvalidFormatError, "Expected chunk ID '#{CHUNK_IDS[:riff]}', but was '#{riff_header[:chunk_id]}'"
      end

      unless riff_header[:riff_format] == WAVEFILE_FORMAT_CODE
        raise_error InvalidFormatError, "Expected RIFF format of '#{WAVEFILE_FORMAT_CODE}', but was '#{riff_header[:riff_format]}'"
      end

      riff_header
    end

    def read_format_chunk(chunk_id, chunk_size)
      if chunk_size < FORMAT_CHUNK_MINIMUM_SIZE
        raise_error InvalidFormatError, "The format chunk is incomplete."
      end

      raw_bytes = read_chunk_body(CHUNK_IDS[:format], chunk_size)

      format_chunk = {}
      format_chunk[:audio_format],
      format_chunk[:channels],
      format_chunk[:sample_rate],
      format_chunk[:byte_rate],
      format_chunk[:block_align],
      format_chunk[:bits_per_sample] = raw_bytes.slice!(0...FORMAT_CHUNK_MINIMUM_SIZE).unpack("vvVVvv")

      if chunk_size > FORMAT_CHUNK_MINIMUM_SIZE
        format_chunk[:extension_size] = raw_bytes.slice!(0...2).unpack(UNSIGNED_INT_16).first

        if format_chunk[:extension_size] == nil
          raise_error InvalidFormatError, "The format chunk is missing an expected extension."
        end

        if format_chunk[:extension_size] != raw_bytes.length
          raise_error InvalidFormatError, "The format chunk extension is shorter than expected."
        end

        # TODO: Parse the extension
      end

      format_chunk
    end

    def read_chunk_body(chunk_id, chunk_size)
      begin
        return @file.sysread(chunk_size)
      rescue EOFError
        raise_error InvalidFormatError, "The #{chunk_id} chunk has incomplete data."
      end
    end

    def raise_error(exception_class, message)
      raise exception_class, "File '#{@file_name}' is not a supported wave file. #{message}"
    end
  end
end
