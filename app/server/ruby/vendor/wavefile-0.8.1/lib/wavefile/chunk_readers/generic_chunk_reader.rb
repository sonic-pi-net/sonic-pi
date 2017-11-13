module WaveFile
  module ChunkReaders
    # Internal
    class GenericChunkReader < BaseChunkReader    # :nodoc:
      def initialize(io, chunk_size)
        @io = io
        @chunk_size = chunk_size
      end

      def read
        @io.sysread(@chunk_size)
      end
    end
  end
end
