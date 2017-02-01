module WaveFile
  module ChunkReaders
    # Internal
    class BaseChunkReader    # :nodoc:
      def raise_error(exception_class, message)
        raise exception_class, "Not a supported wave file. #{message}"
      end
    end
  end
end
