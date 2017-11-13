module WaveFile
  module ChunkReaders
    # Internal: Used to read the RIFF chunks in a wave file up until the data chunk. Thus it
    # can be used to open a wave file and "queue it up" to the start of the actual sample data,
    # as well as extract information out of pre-data chunks, such as the format chunk.
    class RiffReader    # :nodoc:
      def initialize(io, format=nil)
        @io = io

        read_until_data_chunk(format)
      end

      attr_reader :native_format, :data_chunk_reader

    private

      def read_until_data_chunk(format)
        begin
          chunk_id, chunk_size = read_chunk_header
          unless chunk_id == CHUNK_IDS[:riff]
            raise_error InvalidFormatError, "Expected chunk ID '#{CHUNK_IDS[:riff]}', but was '#{chunk_id}'"
          end
          RiffChunkReader.new(@io, chunk_size).read

          chunk_id, chunk_size = read_chunk_header
          while chunk_id != CHUNK_IDS[:data]
            if chunk_id == CHUNK_IDS[:format]
              @native_format = FormatChunkReader.new(@io, chunk_size).read
            else
              # Other chunk types besides the format chunk are ignored. This may change in the future.
              GenericChunkReader.new(@io, chunk_size).read
            end

            # The RIFF specification requires that each chunk be aligned to an even number of bytes,
            # even if the byte count is an odd number.
            #
            # See http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/riffmci.pdf, page 11.
            if chunk_size.odd?
              @io.sysread(1)
            end

            chunk_id, chunk_size = read_chunk_header
          end
        rescue EOFError
          raise_error InvalidFormatError, "It doesn't have a data chunk."
        end

        if @native_format == nil
          raise_error InvalidFormatError, "The format chunk is either missing, or it comes after the data chunk."
        end

        @data_chunk_reader = DataChunkReader.new(@io, chunk_size, @native_format, format)
      end

      def read_chunk_header
        chunk_id = @io.sysread(4)
        chunk_size = @io.sysread(4).unpack(UNSIGNED_INT_32).first || 0

        return chunk_id, chunk_size
      end

      def raise_error(exception_class, message)
        raise exception_class, "Not a supported wave file. #{message}"
      end
    end
  end
end
