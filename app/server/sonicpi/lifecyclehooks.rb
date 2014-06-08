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
  class LifeCycleHooks

    # Life cycle hooks for jobs

    def initialize
      @started_blocks = []
      @completed_blocks = []
      @killed_blocks = []
      @exited_blocks = []
      @mut = Mutex.new
    end

    def on_init(&blck)
      @mut.synchronize do
        @started_blocks << blck
      end
    end

    def on_completed(&blck)
      @mut.synchronize do
        @completed_blocks << blck
      end
    end

    def on_killed(&blck)
      @mut.synchronize do
        @killed_blocks << blck
      end
    end

    def on_exit(&blck)
      @mut.synchronize do
        @exited_blocks << blck
      end
    end

    def init(job_id, arg={})
      @started_blocks.each do |b|
        b.call(job_id, arg)
      end
    end

    def completed(job_id, arg={})
      @completed_blocks.each do |b|
        b.call(job_id, arg)
      end
    end

    def killed(job_id, arg={})
      @killed_blocks.each do |b|
        b.call(job_id, arg)
      end
    end

    def exit(job_id, arg={})
      @exited_blocks.each do |b|
        b.call(job_id, arg)
      end
    end
  end
end
