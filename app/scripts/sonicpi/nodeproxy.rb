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
  class SynthNodeProxy < SynthNode

    def initialize(node)
      @node = node
    end

    def on_destroyed(&block)
      @node.on_destroyed(&block)
    end

    def on_started(&block)
      @node.on_started(&block)
    end

    def kill(now=false)
      @node.kill(now)
    end

    def pause(now=false)
      @node.pause(now)
    end

    def run(now=false)
      @node.run(now)
    end

    def ctl(*args)
      @node.ctl(*args)
    end

    def control(*args)
      @node.ctl(*args)
    end

    def live?
      @node.live?
    end

    def destroyed?
      @node.destroyed?
    end

    def paused?
      @node.paused?
    end

    def running?
      @node.running?
    end

    def state
      @node.state
    end

    def name
      @node.name
    end

    def to_i
      @node.to_i
    end

    def to_f
      @node.to_f
    end

    def to_s
      "#<SonicPi::SynthNodeProxy @id=#{@node.id}>"
    end

    def inspect
      to_s
    end

    def blank_node?
      false
    end
  end
end
