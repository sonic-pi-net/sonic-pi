#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++
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
