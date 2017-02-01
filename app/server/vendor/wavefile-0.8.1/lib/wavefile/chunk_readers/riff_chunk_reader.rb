module WaveFile
  module ChunkReaders
    # Internal
    class RiffChunkReader < BaseChunkReader    # :nodoc:
      def initialize(io, chunk_size)
        @io = io
        @chunk_size = chunk_size
      end

      def read
        riff_format = @io.sysread(4)

        unless riff_format == WAVEFILE_FORMAT_CODE
          raise_error InvalidFormatError, "Expected RIFF format of '#{WAVEFILE_FORMAT_CODE}', but was '#{riff_format}'"
        end
      end
    end
  end
end
