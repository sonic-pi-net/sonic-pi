module Parser

  class StaticEnvironment
    def initialize
      reset
    end

    def reset
      @variables = Set[]
      @stack     = []
    end

    def extend_static
      @stack.push(@variables)
      @variables = Set[]

      self
    end

    def extend_dynamic
      @stack.push(@variables)
      @variables = @variables.dup

      self
    end

    def unextend
      @variables = @stack.pop

      self
    end

    def declare(name)
      @variables.add(name.to_sym)

      self
    end

    def declared?(name)
      @variables.include?(name.to_sym)
    end
  end

end
