#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require_relative "synthnode"

module SonicPi
  class LazyNode < SynthNode

    def initialize(prom)
      @realised = false
      @prom_mut = Mutex.new
      @node = nil
      @prom = prom
    end

    def args
      wait_for_prom unless @realised
      @node.args
    end

    def name
      wait_for_prom unless @realised
      @node.name
    end

    def on_destroyed(&block)
      wait_for_prom unless @realised
      @node.on_destroyed(&block)
    end

    def on_started(&block)
      wait_for_prom unless @realised
      @node.on_started(&block)
    end

    def on_next_move(&block)
      wait_for_prom unless @realised
      @node.on_next_move(&block)
    end

    def move(new_group, pos=nil)
      wait_for_prom unless @realised
      @node.move(new_group, pos)
    end

    def kill(now=false)
      wait_for_prom unless @realised
      @node.kill(now)
    end

    def pause(now=false)
      wait_for_prom unless @realised
      @node.pause(now)
    end

    def run(now=false)
      wait_for_prom unless @realised
      @node.run(now)
    end

    def ctl(*args)
      wait_for_prom unless @realised
      @node.ctl(*args)
    end

    def control(*args)
      wait_for_prom unless @realised
      @node.ctl(*args)
    end

    def ctl_now(*args)
      wait_for_prom unless @realised
      @node.ctl_now(*args)
    end

    def live?
      wait_for_prom unless @realised
      @node.live?
    end

    def destroyed?
      wait_for_prom unless @realised
      @node.destroyed?
    end

    def paused?
      wait_for_prom unless @realised
      @node.paused?
    end

    def running?
      wait_for_prom unless @realised
      @node.running?
    end

    def state
      wait_for_prom unless @realised
      @node.state
    end

    def to_i
      wait_for_prom unless @realised
      @node.to_i
    end

    def to_f
      wait_for_prom unless @realised
      @node.to_f
    end

    def to_s
      wait_for_prom unless @realised
      "#<SonicPi::LazySynthNode @id=#{@node.id}>"
    end

    def id
      wait_for_prom unless @realised
      @node.id
    end

    def inspect
      wait_for_prom unless @realised
      to_s
    end

    def blank_node?
      false
    end

    def info
      wait_for_prom unless @realised
      @node.info
    end


    private

    def wait_for_prom
      return true if @realised
      @prom_mut.synchronize do
        return true if @realised
        @node = @prom.get
      end
    end
  end
end
