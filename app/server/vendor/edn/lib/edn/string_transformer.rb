module EDN
  module StringTransformer
    # Unescape characters in strings.
    # Borrowed from json-pure gem.
    UNESCAPE_MAP = Hash.new { |h, k| h[k] = k.chr }
    UNESCAPE_MAP.update({ ?"  => '"',
                          ?\\ => '\\',
                          ?/  => '/',
                          #' Clear messed up syntax highlighting with Emacs.
                          ?b  => "\b",
                          ?f  => "\f",
                          ?n  => "\n",
                          ?r  => "\r",
                          ?t  => "\t",
                          ?u  => nil,
                        })

    EMPTY_8BIT_STRING = ''
    if ::String.method_defined?(:encode)
      EMPTY_8BIT_STRING.force_encoding Encoding::ASCII_8BIT
    end

    def self.parse_string(string)
      string = string.to_s
      return '' if string.empty?
      string = string.gsub(%r((?:\\[\\bfnrt"/]|(?:\\u(?:[A-Fa-f\d]{4}))+|\\[\x20-\xff]))n) do |c|
        #" Clear messed up syntax highlighting with Emacs.
        if u = UNESCAPE_MAP[$&[1]]
          u
        else # \uXXXX
          bytes = EMPTY_8BIT_STRING.dup
          i = 0
          while c[6 * i] == ?\\ && c[6 * i + 1] == ?u
            bytes << c[6 * i + 2, 2].to_i(16) << c[6 * i + 4, 2].to_i(16)
            i += 1
          end
          JSON.iconv('utf-8', 'utf-16be', bytes)
        end
      end
      if string.respond_to?(:force_encoding)
        string.force_encoding(::Encoding::UTF_8)
      end
      string
    end
  end
end
