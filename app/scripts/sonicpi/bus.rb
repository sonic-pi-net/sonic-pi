module SonicPi
  class Bus
    attr_reader :id, :size
    def initialize(id, size, allocator, &free_fn)
      @id = id
      @size = size
      @allocator = allocator
      @live = true
      @sem = Mutex.new
      @free_fn = free_fn
    end

    def rate
      raise "Unknown rate - inherit and implement me!"
    end

    def free
      @sem.synchronize do
        if @live
          @free_fn.call(@id, @size)
          @live = false
          true
        else
          false
        end
      end
    end

    def to_i
      @id
    end

    def to_f
      @id.to_f
    end

    def to_s
      "#<SonicPi::Bus @id=#{@id}, @size=#{@size}>"
    end
  end
end
