module SonicPi
  class SPSym

    @@cache = {}

    def self.new(value)
      if @@cache[value]
        @@cache[value]
      else
        @@cache[value] = super(value)
      end
    end

    attr_reader :path

    def initialize(path)
      @path = path.freeze
    end.freeze

    def to_s
      inspect
    end

    def inspect
      "<#SPSym #{@path}>"
    end
  end
end
