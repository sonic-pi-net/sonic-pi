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

    def initialize(path_str)
      @path = path_str.split(' : ').map(&:to_sym).freeze
      @path_str = (':' + @path.join(':')).freeze
    end.freeze

    def to_s
      inspect
    end

    def inspect
      @path_str
    end
  end
end
