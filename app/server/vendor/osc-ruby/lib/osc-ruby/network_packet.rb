module OSC
  class NetworkPacket
    def initialize(str)
      @str, @index = str.force_encoding("BINARY"), 0
    end

    def to_s
      @str
    end

    def rem()
      @str.length - @index
    end

    def eof? ()
      rem <= 0
    end

    def skip(n)
      @index += n
    end

    def skip_padding()
      skip((4 - (@index % 4)) % 4)
    end

    def getn(n)
    	raise EOFError if rem < n
    	s = @str[@index, n]
    	skip(n)
    	s
    end

    def getc
    	raise EOFError if rem < 1
    	c = @str[@index]
    	skip(1)
    	c
    end
  end
end