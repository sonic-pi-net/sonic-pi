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
require_relative "node"

module SonicPi
  class BlankNode < Node

    def initialize()
    end

    def on_destroyed(&block)
    end

    def on_started(&block)
    end

    def kill(now=false)
      self
    end

    def pause(now=false)
      self
    end

    def run(now=false)
      self
    end

    def ctl(*args)
      self
    end

    def control(*args)
      self
    end

    def live?
      false
    end

    def destroyed?
      true
    end

    def paused?
      false
    end

    def running?
      false
    end

    def state
      :destroyed
    end

    def name
      ""
    end

    def to_i
      -1
    end

    def to_f
      -1.0
    end

    def to_s
      "#<SonicPi::BlankNode>"
    end

    def inspect
      to_s
    end

    def blank_node?
      true
    end
  end
end
