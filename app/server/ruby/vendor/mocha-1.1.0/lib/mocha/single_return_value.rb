require 'mocha/is_a'

module Mocha

  class SingleReturnValue

    def initialize(value)
      @value = value
    end

    def evaluate
      @value
    end

  end

end
