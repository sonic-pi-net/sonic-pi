require 'thread'

module SonicPi
  class Promise

    def initialize
      @val_sem = Mutex.new
      @push_sem = Mutex.new
      @box = Queue.new
      @value = nil
      @delivered = false
      @pushed = false
    end

    def get
      return @value if @delivered
      @val_sem.synchronize do
        return @value if @delivered
        val = @box.pop
        @value = val
        @delivered = true
        val
      end
    end

    def deliver!(val)
      @push_sem.synchronize do
        if(@box.empty? && !@pushed)
          @box.push val
          @pushed = true
        else
          raise "Promise already delivered"
        end
      end
    end
  end
end
