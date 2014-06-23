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
module SonicPi
  class FXNode < ChainNode

    def initialize(node, in_bus, out_bus)
      super(node)
      @in_bus = in_bus
      @out_bus = out_bus
      @node.on_destroyed do
        @in_bus.free
      end
    end

    def to_s
      i = @in_bus ? @in_bus.id : nil
      o = @out_bus ? @out_bus.id : nil
      "#<SonicPi::FXNode @id=#{@node.id}, @in_bus=#{i.inspect}, @out_bus=#{o.inspect}>"
    end

  end
end
