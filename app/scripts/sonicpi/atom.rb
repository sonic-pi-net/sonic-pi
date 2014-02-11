require 'thread'

## Assumes Immutable values used as val...
module SonicPi
  class Atom
    def initialize(val)
      @val = val
      @sem = Mutex.new
    end

    def swap!(&block)
      Thread.current.thread_variable_set :sonic_pi_atom_last, @val
      Thread.current.thread_variable_set :sonic_pi_atom_new, block.call(@val)
      @sem.synchronize do
        if @val == Thread.current.thread_variable_get(:sonic_pi_atom_last)
          @val = Thread.current.thread_variable_get(:sonic_pi_atom_new)
          return @val
        end
      end

      ## Didn't work, try again...
      swap!(&block)
    end

    def swap_returning_old!(&block)
      Thread.current.thread_variable_set :sonic_pi_atom_last, @val
      Thread.current.thread_variable_set :sonic_pi_atom_new, block.call(@val)
      @sem.synchronize do
        last = Thread.current.thread_variable_get(:sonic_pi_atom_last)
        if @val == last
          @val = Thread.current.thread_variable_get(:sonic_pi_atom_new)
          return last
        end
      end

      ## Didn't work, try again...
      swap_returning_old!(&block)
    end

    def deref
      @val
    end

    def reset!(new_val)
      @sem.synchronize do
        @val = new_val
      end
    end
  end
end
