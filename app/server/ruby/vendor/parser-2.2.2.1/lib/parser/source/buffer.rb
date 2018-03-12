# encoding: ascii-8bit

module Parser
  module Source

    ##
    # A buffer with source code. {Buffer} contains the source code itself,
    # associated location information (name and first line), and takes care
    # of encoding.
    #
    # A source buffer is immutable once populated.
    #
    # @!attribute [r] name
    #  Buffer name. If the buffer was created from a file, the name corresponds
    #  to relative path to the file.
    #  @return [String] buffer name
    #
    # @!attribute [r] first_line
    #  First line of the buffer, 1 by default.
    #  @return [Integer] first line
    #
    # @api public
    #
    class Buffer
      attr_reader :name, :first_line

      ##
      # @api private
      #
      ENCODING_RE =
        /\#.*coding\s*[:=]\s*
          (
            # Special-case: there's a UTF8-MAC encoding.
            (utf8-mac)
          |
            # Chew the suffix; it's there for emacs compat.
            ([A-Za-z0-9_-]+?)(-unix|-dos|-mac)
          |
            ([A-Za-z0-9_-]+)
          )
        /x

      ##
      # Try to recognize encoding of `string` as Ruby would, i.e. by looking for
      # magic encoding comment or UTF-8 BOM. `string` can be in any encoding.
      #
      # @param [String]  string
      # @return [String|nil] encoding name, if recognized
      #
      def self.recognize_encoding(string)
        return if string.empty?

        # extract the first two lines in an efficient way
        string =~ /\A(.*)\n?(.*\n)?/
        first_line, second_line = $1, $2

        if first_line =~ /\A\xef\xbb\xbf/ # BOM
          return Encoding::UTF_8
        elsif first_line[0, 2] == '#!'
          encoding_line = second_line
        else
          encoding_line = first_line
        end

        if (result = ENCODING_RE.match(encoding_line))
          Encoding.find(result[2] || result[3] || result[5])
        else
          nil
        end
      end

      ##
      # Recognize encoding of `input` and process it so it could be lexed.
      #
      #  * If `input` does not contain BOM or magic encoding comment, it is
      #    kept in the original encoding.
      #  * If the detected encoding is binary, `input` is kept in binary.
      #  * Otherwise, `input` is re-encoded into UTF-8 and returned as a
      #    new string.
      #
      # This method mutates the encoding of `input`, but not its content.
      #
      # @param  [String] input
      # @return [String]
      # @raise  [EncodingError]
      #
      def self.reencode_string(input)
        original_encoding = input.encoding
        detected_encoding = recognize_encoding(input.force_encoding(Encoding::BINARY))

        if detected_encoding.nil?
          input.force_encoding(original_encoding)
        elsif detected_encoding == Encoding::BINARY
          input
        else
          input.
            force_encoding(detected_encoding).
            encode(Encoding::UTF_8)
        end
      end

      def initialize(name, first_line = 1)
        @name        = name
        @source      = nil
        @first_line  = first_line

        @lines       = nil
        @line_begins = nil
      end

      ##
      # Populate this buffer from correspondingly named file.
      #
      # @example
      #  Parser::Source::Buffer.new('foo/bar.rb').read
      #
      # @return [Buffer] self
      # @raise  [ArgumentError] if already populated
      #
      def read
        File.open(@name, 'rb') do |io|
          self.source = io.read
        end

        self
      end

      ##
      # Source code contained in this buffer.
      #
      # @return [String] source code
      # @raise  [RuntimeError] if buffer is not populated yet
      #
      def source
        if @source.nil?
          raise RuntimeError, 'Cannot extract source from uninitialized Source::Buffer'
        end

        @source
      end

      ##
      # Populate this buffer from a string with encoding autodetection.
      # `input` is mutated if not frozen.
      #
      # @param [String] input
      # @raise [ArgumentError] if already populated
      # @raise [EncodingError] if `input` includes invalid byte sequence for the encoding
      # @return [String]
      #
      def source=(input)
        if defined?(Encoding)
          input = input.dup if input.frozen?
          input = self.class.reencode_string(input)

          unless input.valid_encoding?
            raise EncodingError, "invalid byte sequence in #{input.encoding.name}"
          end
        end

        self.raw_source = input
      end

      ##
      # Populate this buffer from a string without encoding autodetection.
      #
      # @param [String] input
      # @raise [ArgumentError] if already populated
      # @return [String]
      #
      def raw_source=(input)
        if @source
          raise ArgumentError, 'Source::Buffer is immutable'
        end

        @source = input.gsub("\r\n", "\n").freeze
      end

      ##
      # Convert a character index into the source to a `[line, column]` tuple.
      #
      # @param  [Integer] position
      # @return [[Integer, Integer]] `[line, column]`
      #
      def decompose_position(position)
        line_no, line_begin = line_for(position)

        [ @first_line + line_no, position - line_begin ]
      end

      ##
      # Extract line `lineno` from source, taking `first_line` into account.
      #
      # @param  [Integer] lineno
      # @return [String]
      # @raise  [IndexError] if `lineno` is out of bounds
      #
      def source_line(lineno)
        unless @lines
          @lines = @source.lines.to_a
          @lines.each { |line| line.chomp!("\n") }

          # If a file ends with a newline, the EOF token will appear
          # to be one line further than the end of file.
          @lines << ""
        end

        @lines.fetch(lineno - @first_line).dup
      end

      private

      def line_begins
        unless @line_begins
          @line_begins, index = [ [ 0, 0 ] ], 1

          @source.each_char do |char|
            if char == "\n"
              @line_begins.unshift [ @line_begins.length, index ]
            end

            index += 1
          end
        end

        @line_begins
      end

      def line_for(position)
        if line_begins.respond_to? :bsearch
          # Fast O(log n) variant for Ruby >=2.0.
          line_begins.bsearch do |line, line_begin|
            line_begin <= position
          end
        else
          # Slower O(n) variant for Ruby <2.0.
          line_begins.find do |line, line_begin|
            line_begin <= position
          end
        end
      end
    end

  end
end
