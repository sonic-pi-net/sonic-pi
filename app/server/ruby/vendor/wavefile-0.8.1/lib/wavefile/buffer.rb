module WaveFile
  # Public: Error that is raised when an attempt is made to perform an unsupported or
  # undefined conversion between two sample data formats. For example, converting a Buffer
  # with 3 channels into a Buffer with 2 channels is undefined.
  class BufferConversionError < StandardError; end


  # Public: Represents a collection of samples in a certain format (e.g. 16-bit mono).
  # Reader returns sample data contained in Buffers, and Writer expects incoming sample
  # data to be contained in a Buffer as well.
  #
  # Contains methods to convert the sample data in the buffer to a different format.
  class Buffer

    # Public: Creates a new Buffer.
    #
    # samples - An array of samples. If the Format has 1 channel (i.e. is mono), this
    #           should be a flat array of samples such as [0.5, 0.4, -0.3, ...]. If the
    #           Format has 2 or more channels the array should include a sub-array for
    #           each sample frame. For example, [[0.5, 0.2], [0.1, 0.6], [-0.2, 0.4], ...]
    #           for a stereo file.
    #
    #           The individual samples should match the given format:
    #
    #           :pcm_8    - Integer between 0 and 255
    #           :pcm_16   - Integer between -32_768 and 32_767
    #           :pcm_24   - Integer between -8_388_608 and 8_388_607
    #           :pcm_32   - Integer between 2_147_483_648 and 2_147_483_647
    #           :float    - Float between -1.0 and 1.0
    #           :float_32 - Float between -1.0 and 1.0
    #           :float_64 - Float between -1.0 and 1.0
    #
    # format - A Format instance which describes the sample format of the sample array.
    #
    #          Note that the sample array is not compared with the format to make sure
    #          they match - you are on the honor system to make sure they do. If they
    #          don't match, unexpected things will happen.
    #
    # Examples
    #
    #   samples = ([0.5] * 50) + ([-0.5] * 50)   # A floating point 440Hz mono square wave
    #   buffer = Buffer.new(samples, Format.new(:mono, :float, 44100)
    #
    #   samples = ([0.5, 0.5] * 50) + ([-0.5, -0.5] * 50)   # A 440Hz stereo square wave
    #   buffer = Buffer.new(samples, Format.new(2, :float, 44100)
    #
    #   samples = ([16000] * 50) + ([-16000] * 50)   # A 16-bit PCM 440Hz mono square wave
    #   buffer = Buffer.new(samples, Format.new(1, :pcm_16, 44100)
    #
    # Returns a constructed Buffer.
    def initialize(samples, format)
      @samples = samples
      @format = format
    end


    # Public: Creates a new Buffer containing the sample data of this Buffer, but converted
    # to a different format.
    #
    # new_format - The format that the sample data should be converted to
    #
    # Examples
    #
    #   new_format = Format.new(:mono, :pcm_16, 44100)
    #   new_buffer = old_buffer.convert(new_format)
    #
    # Returns a new Buffer; the existing Buffer is unmodified.
    # Raises BufferConversionError if the Buffer can't be converted to the given format
    def convert(new_format)
      new_samples = convert_buffer(@samples.dup, @format, new_format)
      Buffer.new(new_samples, new_format)
    end


    # Public: Converts the sample data contained in the Buffer to a new format. The sample
    # data is converted in place, so the existing Buffer is modified.
    #
    # new_format - The format that the sample data should be converted to
    #
    # Examples
    #
    #   new_format = Format.new(:mono, :pcm_16, 44100)
    #   old_buffer.convert!(new_format)
    #
    # Returns self.
    # Raises BufferConversionError if the Buffer can't be converted to the given format
    def convert!(new_format)
      @samples = convert_buffer(@samples, @format, new_format)
      @format = new_format
      self
    end


    # Public: Returns the number of channels the buffer's sample data has
    def channels
      @format.channels
    end


    # Public: Returns the bits per sample of the buffer's sample data
    def bits_per_sample
      @format.bits_per_sample
    end


    # Public: Returns the sample rate of the buffer's sample data
    def sample_rate
      @format.sample_rate
    end

    # Public: Returns the sample data contained in the Buffer as an Array. If the Format
    # has 1 channel, the Array will be a flat list of samples. If the Format has 2 or
    # more channels, the Array will include sub arrays for each sample frame, with a sample
    # for each channel.
    #
    # Examples
    #
    #   samples = mono_buffer.samples
    #   # => [-0.5, 0.3, 0.2, -0.9, ...]
    #
    #   samples = stereo_buffer.samples
    #   # => [[-0.2, 0.5], [0.1, 0.2], [-0.4, 0.7], [0.1, 0.2], ...]
    #
    #   samples = three_channel_buffer.samples
    #   # => [[0.3, 0.5, 0.2], [-0.1, 0.2, -0.9], [0.2, 0.3, -0.4], [0.1, 0.2, -0.8], ...]
    attr_reader :samples

  private

    def convert_buffer(samples, old_format, new_format)
      if old_format.channels > new_format.channels
        samples = convert_channels(samples, old_format.channels, new_format.channels)
        samples = convert_sample_format(samples, old_format, new_format)
      else
        samples = convert_sample_format(samples, old_format, new_format)
        samples = convert_channels(samples, old_format.channels, new_format.channels)
      end

      samples
    end

    def convert_channels(samples, old_channels, new_channels)
      return samples if old_channels == new_channels

      # The cases of mono -> stereo and vice-versa are handled specially,
      # because those conversion methods are faster than the general methods,
      # and the large majority of wave files are expected to be either mono or stereo.
      if old_channels == 1 && new_channels == 2
        samples.map! {|sample| [sample, sample]}
      elsif old_channels == 2 && new_channels == 1
        samples.map! {|sample| (sample[0] + sample[1]) / 2}
      elsif old_channels == 1 && new_channels >= 2
        samples.map! {|sample| [].fill(sample, 0, new_channels)}
      elsif old_channels >= 2 && new_channels == 1
        samples.map! {|sample| sample.inject(0) {|sub_sample, sum| sum + sub_sample } / old_channels }
      elsif old_channels > 2 && new_channels == 2
        samples.map! {|sample| [sample[0], sample[1]]}
      else
        raise BufferConversionError,
              "Conversion of sample data from #{old_channels} channels to #{new_channels} channels is unsupported"
      end

      samples
    end

    def convert_sample_format(samples, old_format, new_format)
      return samples if old_format.sample_format == :float && new_format.sample_format == :float

      if old_format.sample_format == :pcm && new_format.sample_format == :pcm
        convert_sample_format_pcm_to_pcm(samples, old_format.bits_per_sample, new_format.bits_per_sample)
      elsif old_format.sample_format == :pcm && new_format.sample_format == :float
        convert_sample_format_pcm_to_float(samples, old_format.bits_per_sample, new_format.bits_per_sample)
      elsif old_format.sample_format == :float && new_format.sample_format == :pcm
        convert_sample_format_float_to_pcm(samples, old_format.bits_per_sample, new_format.bits_per_sample)
      end
    end

    def convert_sample_format_pcm_to_pcm(samples, old_bits_per_sample, new_bits_per_sample)
      return samples if old_bits_per_sample == new_bits_per_sample

      shift_amount = (new_bits_per_sample - old_bits_per_sample).abs

      if old_bits_per_sample == 8
        convert_sample_format_helper(samples) {|sample| (sample - 128) << shift_amount }
      elsif new_bits_per_sample == 8
        convert_sample_format_helper(samples) {|sample| (sample >> shift_amount) + 128 }
      else
        if new_bits_per_sample > old_bits_per_sample
          convert_sample_format_helper(samples) {|sample| sample << shift_amount }
        else
          convert_sample_format_helper(samples) {|sample| sample >> shift_amount }
        end
      end
    end

    def convert_sample_format_pcm_to_float(samples, old_bits_per_sample, new_bits_per_sample)
      if old_bits_per_sample == 8
        convert_sample_format_helper(samples) {|sample| (sample - 128).to_f / 128.0 }
      elsif old_bits_per_sample == 16
        convert_sample_format_helper(samples) {|sample| sample.to_f / 32768.0 }
      elsif old_bits_per_sample == 24
        convert_sample_format_helper(samples) {|sample| sample.to_f / 8388608.0 }
      elsif old_bits_per_sample == 32
        convert_sample_format_helper(samples) {|sample| sample.to_f / 2147483648.0 }
      end
    end

    def convert_sample_format_float_to_pcm(samples, old_bits_per_sample, new_bits_per_sample)
      if new_bits_per_sample == 8
        convert_sample_format_helper(samples) {|sample| (sample * 127.0).round + 128 }
      elsif new_bits_per_sample == 16
        convert_sample_format_helper(samples) {|sample| (sample * 32767.0).round }
      elsif new_bits_per_sample == 24
        convert_sample_format_helper(samples) {|sample| (sample * 8388607.0).round }
      elsif new_bits_per_sample == 32
        convert_sample_format_helper(samples) {|sample| (sample * 2147483647.0).round }
      end
    end

    def convert_sample_format_helper(samples, &converter)
      more_than_one_channel = (Array === samples.first)

      if more_than_one_channel
        samples.map! do |sample|
          sample.map!(&converter)
        end
      else
        samples.map!(&converter)
      end
    end
  end
end
