require 'thread'

module SonicPi
  class Counter
    def initialize(init_val=0)
      @counter_sem = Mutex.new
      @current_val = init_val
    end

    def next
      @counter_sem.synchronize do
        @current_val += 1
      end
    end
  end
end
