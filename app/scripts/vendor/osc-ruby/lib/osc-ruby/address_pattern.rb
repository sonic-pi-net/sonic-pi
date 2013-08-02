module OSC
  class AddressPattern
    def initialize( pattern )
      @pattern = pattern

      generate_regex_from_pattern
    end

    def match?( address )
      !!(@re.nil? || @re.match( address ))
    end

private
    def generate_regex_from_pattern
      case @pattern
        when NIL; @re = @pattern
        when Regexp; @re = @pattern
        when String

          # i'm unsure what this does
          # @pattern.gsub!(/[.^(|)]/, '\\1')

          # handles osc single char wildcard matching
          @pattern.gsub!(/\?/, '[^/]')

          # handles ** - used in creating osc routers
          # TODO: turns out the OSC 1.1 spec says that we shoudl use "//" 
          # this will be implemented in the next major release of this gem
          #@pattern.gsub!(/\*\*/, '.*' )

          # handles osc * - 0 or more matching
          @pattern.gsub!(/\*[^\*]/, '[^/]*')

          # handles [!] matching
          @pattern.gsub!(/\[!/, '[^')

          # handles {} matching
          @pattern.gsub!(/\{/, '(')
          @pattern.gsub!(/,/, '|')
          @pattern.gsub!(/\}/, ')')


          # keeps from matching before the begining of the pattern
          @pattern.gsub!(/\A/, '\A')

          # keeps from matching beyond the end,
          # eg. pattern /hi does not match /hidden
          @pattern.gsub!(/\z/, '\z')

          @re = Regexp.new(@pattern)
        else
          raise ArgumentError, 'invalid pattern'
      end
    end
  end
end
