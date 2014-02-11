require 'thread'
require_relative 'atom'

module SonicPi
  class Counter
    def initialize(init_val=0)
      @INIT_VAL = init_val
      @current_val_A = Atom.new(init_val)
    end

    def next
      @current_val_A.swap!{|el| el + 1}
    end

    def reset!
      @current_val_A.reset! @INIT_VAL
    end
  end
end
