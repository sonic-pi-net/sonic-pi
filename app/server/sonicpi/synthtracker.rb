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
require 'set'

require_relative "promise"

module SonicPi
  class SynthTracker

    def initialize
      @synths = Set.new
      @mut = Mutex.new
      @prom = nil
    end

    def synth_started(s)
      @mut.synchronize do
        @synths << s
      end
    end

    def synth_finished(s)
      @mut.synchronize do
        @synths.delete s
        if @prom && @synths.empty?
          @prom.deliver! true, false
        end
      end
    end

    def block_until_finished
      @mut.synchronize do
        @prom = Promise.new
        if @synths.empty?
          @prom.deliver! true, false
        end
      end
      @prom.get
    end
  end
end
