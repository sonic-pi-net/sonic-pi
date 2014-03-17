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
      raise "blocking underway - no more synths should be created!" if @prom
      @mut.synchronize do
        puts "synth started: #{s}"
        @synths << s
        puts "synth added: #{@synths.to_a}"
      end
    end

    def synth_finished(s)
      @mut.synchronize do
        puts "synth finished: #{s}"
        @synths.delete s
        if @prom && @synths.empty?
          puts "delivered!"
          @prom.deliver! true
        end
      end
    end

    def block_until_finished
      puts "blocking..."
      @mut.synchronize do
        @prom = Promise.new
        puts "blocking with synths: #{@synths.to_a}"
        if @synths.empty?
          @prom.deliver! true
        end
      end
      puts "actually blocking now..."
      @prom.get
      puts "finished blocking!"
    end
  end
end
