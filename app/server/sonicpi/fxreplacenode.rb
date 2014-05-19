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
  class FXReplaceNode < SynthNodeProxy

    def initialize(node)
      super(node)
      @audio_bus = node.args["out_bus"]
    end

    def to_s
      "#<SonicPi::FXReplaceNode @id=#{@node.id}, @audio_bus=#{@audio_bus.inspect}}>"
    end

  end
end
