module RBeautify

  class BlockEnd

    attr_accessor :block_start, :offset, :match, :after_match

    def initialize(block_start, offset, match, after_match)
      self.block_start = block_start
      self.offset = offset
      self.match = match
      self.after_match = after_match
    end

    def end_offset
      offset + match.length
    end

    def end_can_also_be_start?
      block_start.block_matcher.end_can_also_be_start?
    end
  end

end
