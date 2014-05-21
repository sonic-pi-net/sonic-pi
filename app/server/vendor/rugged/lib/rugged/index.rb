module Rugged
  class Index
    include Enumerable

    def to_s
      s = "#<Rugged::Index\n"
      self.each do |entry|
        s << "  [#{entry[:stage]}] '#{entry[:path]}'\n"
      end
      s + '>'
    end
  end
end
