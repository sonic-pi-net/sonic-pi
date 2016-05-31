module Rugged
  class Blob
    class HashSignature
      WHITESPACE_DEFAULT  = 0
      WHITESPACE_IGNORE   = 1
      WHITESPACE_SMART    = 2
    end

    def hashsig(options = 0)
      @hashsig ||= HashSignature.new(self, options)
    end

    def similarity(other)
      other_sig = case other
      when HashSignature
        other
      when String
        HashSignature.new(other)
      when Blob
        other.hashsig
      else
        raise TypeError, "Expected a Rugged::Blob, String or Rugged::Blob::HashSignature"
      end

      HashSignature.compare(self.hashsig, other_sig)
    end
  end
end
