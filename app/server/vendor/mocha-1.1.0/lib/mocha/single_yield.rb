module Mocha

  class SingleYield

    attr_reader :parameters

    def initialize(*parameters)
      @parameters = parameters
    end

    def each
      yield(@parameters)
    end

  end

end

