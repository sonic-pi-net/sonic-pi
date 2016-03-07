module WaveFile
  module ChunkReaders
    # Used to read the RIFF chunks in a wave file up until the data chunk. Thus is can be used
    # to open a wave file and "queue it up" to the start of the actual sample data, as well as
    # extract information out of pre-data chunks, such as the format chunk.
    class RiffReader    # :nodoc:
      def initialize(file, file_name)
        @file = file
        @file_name = file_name

        read_until_data_chunk
      end

      attr_reader :native_format, :data_chunk_reader

    private

      def read_until_data_chunk
        begin
          chunk_id = @file.sysread(4)
          unless chunk_id == CHUNK_IDS[:riff]
            raise_error InvalidFormatError, "Expected chunk ID '#{CHUNK_IDS[:riff]}', but was '#{chunk_id}'"
          end
          RiffChunkReader.new(@file).read

          chunk_id = @file.sysread(4)
          while chunk_id != CHUNK_IDS[:data]
            if chunk_id == CHUNK_IDS[:format]
              @native_format = FormatChunkReader.new(@file).read
            else
              # Other chunk types besides the format chunk are ignored. This may change in the future.
              GenericChunkReader.new(@file).read              
            end

            chunk_id = @file.sysread(4)
          end
        rescue EOFError
          raise_error InvalidFormatError, "It doesn't have a data chunk."
        end

        if @native_format == nil
          raise_error InvalidFormatError, "The format chunk is either missing, or it comes after the data chunk."
        end

        @data_chunk_reader = DataChunkReader.new(@file, @native_format)
      end

      class BaseChunkReader    # :nodoc:
        def read_chunk_size
          chunk_size = @file.sysread(4).unpack(UNSIGNED_INT_32).first || 0

          # The RIFF specification requires that each chunk be aligned to an even number of bytes,
          # even if the byte count is an odd number.
          #
          # See http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/riffmci.pdf, page 11.
          if chunk_size.odd?
            chunk_size += 1
          end

          chunk_size
        end

        def raise_error(exception_class, message)
          raise exception_class, "File '#{@file_name}' is not a supported wave file. #{message}"
        end
      end

      class GenericChunkReader < BaseChunkReader    # :nodoc:
        def initialize(file)
          @file = file
        end

        def read
          chunk_size = read_chunk_size
          @file.sysread(chunk_size)
        end
      end

      class RiffChunkReader < BaseChunkReader    # :nodoc:
        def initialize(file)
          @file = file
        end

        def read
          chunk_size = read_chunk_size
          riff_format = @file.sysread(4)

          unless riff_format == WAVEFILE_FORMAT_CODE
            raise_error InvalidFormatError, "Expected RIFF format of '#{WAVEFILE_FORMAT_CODE}', but was '#{riff_format}'"
          end
        end
      end

      class DataChunkReader < BaseChunkReader    # :nodoc:
        def initialize(file, native_format)
          @file = file
          @native_format = native_format

          data_chunk_size = @file.sysread(4).unpack(UNSIGNED_INT_32).first
          @sample_frame_count = data_chunk_size / @native_format.block_align
        end

        attr_reader :sample_frame_count
      end

      class FormatChunkReader < BaseChunkReader    # :nodoc:
        def initialize(file)
          @file = file
        end

        def read
          chunk_size = read_chunk_size

          if chunk_size < MINIMUM_CHUNK_SIZE
            raise_error InvalidFormatError, "The format chunk is incomplete."
          end

          raw_bytes = read_chunk_body(CHUNK_IDS[:format], chunk_size)

          format_chunk = {}
          format_chunk[:audio_format],
          format_chunk[:channels],
          format_chunk[:sample_rate],
          format_chunk[:byte_rate],
          format_chunk[:block_align],
          format_chunk[:bits_per_sample] = raw_bytes.slice!(0...MINIMUM_CHUNK_SIZE).unpack("vvVVvv")

          if chunk_size > MINIMUM_CHUNK_SIZE
            format_chunk[:extension_size] = raw_bytes.slice!(0...2).unpack(UNSIGNED_INT_16).first

            if format_chunk[:extension_size] == nil
              raise_error InvalidFormatError, "The format chunk is missing an expected extension."
            end

            if format_chunk[:extension_size] != raw_bytes.length
              raise_error InvalidFormatError, "The format chunk extension is shorter than expected."
            end

            # TODO: Parse the extension
          end

          UnvalidatedFormat.new(format_chunk)
        end

        private

        MINIMUM_CHUNK_SIZE = 16

        def read_chunk_body(chunk_id, chunk_size)
          begin
            return @file.sysread(chunk_size)
          rescue EOFError
            raise_error InvalidFormatError, "The #{chunk_id} chunk has incomplete data."
          end
        end
      end

      def raise_error(exception_class, message)
        raise exception_class, "File '#{@file_name}' is not a supported wave file. #{message}"
      end
    end
  end
end
