module WaveFile
  module ChunkReaders
    # Internal
    class DataChunkReader < BaseChunkReader    # :nodoc:
      def initialize(io, chunk_size, raw_native_format, format=nil)
        @io = io
        @raw_native_format = raw_native_format

        @total_sample_frames = chunk_size / @raw_native_format.block_align
        @current_sample_frame = 0

        @readable_format = true
        begin
          @native_format = @raw_native_format.to_validated_format
          @pack_code = PACK_CODES[@native_format.sample_format][@native_format.bits_per_sample]
        rescue FormatError
          @readable_format = false
          @native_format = nil
          @pack_code = nil
        end

        @format = (format == nil) ? (@native_format || @raw_native_format) : format
      end

      def read(sample_frame_count)
        raise UnsupportedFormatError unless @readable_format

        if @current_sample_frame >= @total_sample_frames
          #FIXME: Do something different here, because the end of the file has not actually necessarily been reached
          raise EOFError
        elsif sample_frame_count > sample_frames_remaining
          sample_frame_count = sample_frames_remaining
        end

        samples = @io.sysread(sample_frame_count * @native_format.block_align).unpack(@pack_code)
        @current_sample_frame += sample_frame_count

        if @native_format.bits_per_sample == 24
          samples = convert_24_bit_samples(samples)
        end

        if @native_format.channels > 1
          samples = samples.each_slice(@native_format.channels).to_a
        end

        buffer = Buffer.new(samples, @native_format)
        buffer.convert(@format)
      end

      attr_reader :raw_native_format,
                  :format,
                  :current_sample_frame,
                  :total_sample_frames,
                  :readable_format

    private

      # The number of sample frames in the file after the current sample frame
      def sample_frames_remaining
        @total_sample_frames - @current_sample_frame
      end

      # Since Ruby doesn't have a way to natively extract 24-bit values using pack/unpack,
      # unsigned bytes are read instead, and then every 3 is manually combined into a
      # signed 24-bit integer.
      # Since the sample data is little endian, the 3 bytes will go from least->most significant
      def convert_24_bit_samples(samples)
        samples.each_slice(3).map do |least_significant_byte, middle_byte, most_significant_byte|
          # Convert the most significant byte from unsigned to signed, since 24-bit samples are signed
          most_significant_byte = [most_significant_byte].pack("c").unpack("c").first

          (most_significant_byte << 16) | (middle_byte << 8) | least_significant_byte
        end
      end
    end
  end
end
