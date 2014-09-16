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
require_relative "synthnodeproxy"

module SonicPi
  class ChainNode < SynthNodeProxy

    attr_reader :in_bus, :out_bus

    def initialize(node)
      super(node)
      @in_bus = node.args["in_bus"]
      @out_bus = node.args["out_bus"]
    end

    def to_s
      i = @in_bus ? @in_bus : nil
      o = @out_bus? @out_bus : nil
      "#<SonicPi::ChainNode @id=#{@node.id}, @in_bus=#{i.inspect}, @out_bus=#{o.inspect}>"
    end

  end
end
