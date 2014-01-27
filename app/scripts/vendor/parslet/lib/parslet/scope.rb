class Parslet::Scope
  # Raised when the accessed slot has never been assigned a value. 
  #
  class NotFound < StandardError
  end
  
  class Binding
    attr_reader :parent
    
    def initialize(parent=nil)
      @parent = parent
      @hash = Hash.new
    end
    
    def [](k)
      @hash.has_key?(k) && @hash[k] ||
        parent && parent[k] or 
        raise NotFound
    end
    def []=(k,v)
      @hash.store(k,v)
    end
  end
  
  def [](k)
    @current[k]
  end
  def []=(k,v)
    @current[k] = v
  end
  
  def initialize
    @current = Binding.new
  end
  
  def push
    @current = Binding.new(@current)
  end
  def pop
    @current = @current.parent
  end
end