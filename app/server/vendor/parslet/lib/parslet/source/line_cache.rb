

class Parslet::Source
  # A cache for line start positions. 
  #
  class LineCache 
    def initialize
      # Stores line endings as a simple position number. The first line always
      # starts at 0; numbers beyond the biggest entry are on any line > size, 
      # but probably make a scan to that position neccessary.
      @line_ends = []
      @line_ends.extend RangeSearch
    end

    # Returns a <line, column> tuple for the given input position. 
    # 
    def line_and_column(pos)
      eol_idx = @line_ends.lbound(pos)

      if eol_idx
        # eol_idx points to the offset that ends the current line.
        # Let's try to find the offset that starts it: 
        offset = eol_idx>0 && @line_ends[eol_idx-1] || 0
        return [eol_idx+1, pos-offset+1]
      else
        # eol_idx is nil, that means that we're beyond the last line end that
        # we know about. Pretend for now that we're just on the last line.
        offset = @line_ends.last || 0
        return [@line_ends.size+1, pos-offset+1]
      end
    end

    def scan_for_line_endings(start_pos, buf)
      return unless buf

      buf = StringScanner.new(buf)
      return unless buf.exist?(/\n/)

      ## If we have already read part or all of buf, we already know about
      ## line ends in that portion. remove it and correct cur (search index)
      if @last_line_end && start_pos < @last_line_end
        # Let's not search the range from start_pos to last_line_end again.
        buf.pos = @last_line_end - start_pos
      end

      ## Scan the string for line endings; store the positions of all endings
      ## in @line_ends. 
      while buf.skip_until(/\n/)
        @last_line_end = start_pos + buf.pos
        @line_ends << @last_line_end
      end
    end
  end

  # Mixin for arrays that implicitly give a number of ranges, where one range
  # begins where the other one ends.
  # 
  #   Example: 
  #
  #     [10, 20, 30]
  #     # would describe [0, 10], (10, 20], (20, 30]
  #
  module RangeSearch 
    def find_mid(left, right)
      # NOTE: Jonathan Hinkle reported that when mathn is required, just
      # dividing and relying on the integer truncation is not enough.
      left + ((right - left) / 2).floor
    end  
    
    # Scans the array for the first number that is > than bound. Returns the 
    # index of that number. 
    #
    def lbound(bound)
      return nil if empty?
      return nil unless last > bound

      left = 0
      right = size - 1 

      loop do
        mid = find_mid(left, right)

        if self[mid] > bound
          right = mid
        else
          # assert: self[mid] <= bound
          left = mid+1
        end

        if right <= left
          return right
        end
      end
    end
  end
end
