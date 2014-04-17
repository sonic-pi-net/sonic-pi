module EDN
  class Reader
    include Enumerable

    def initialize(text)
      @parser = Parser.new
      @transform = Transform.new
      @original_text = text
      @text = text
    end

    def eof?
      @text.nil? || @text.empty?
    end

    def each
      reset!
      return enum_for(:select) unless block_given?

      until eof?
        yield read
      end
    end

    def reset!
      @text = @original_text
    end

    def read
      raise "EDN::Reader is out of string!" if eof?
      element, rest = @parser.parse_prefix(@text)
      @text = rest
      @transform.apply(element)
    end
  end
end
