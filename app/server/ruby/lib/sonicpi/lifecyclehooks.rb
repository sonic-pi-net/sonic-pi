#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "util"

module SonicPi
  class LifeCycleHooks

    include Util

    # Life cycle hooks for jobs

    def initialize
      @started_blocks = []
      @completed_blocks = []
      @killed_blocks = []
      @exited_blocks = []
      @all_completed_blocks = []
      @mut = Mutex.new
      @started = false
    end

    def ensure_not_started!
      if @started
        raise StandardError "Cannot modify life cycle hooks once started"
      end
    end

    def on_init(&blck)
      @mut.synchronize do
        ensure_not_started!
        @started_blocks << blck
      end
    end

    def on_completed(&blck)
      @mut.synchronize do
        ensure_not_started!
        @completed_blocks << blck
      end
    end

    def on_killed(&blck)
      @mut.synchronize do
        ensure_not_started!
        @killed_blocks << blck
      end
    end

    def on_exit(&blck)
      @mut.synchronize do
        ensure_not_started!
        @exited_blocks << blck
      end
    end

    def on_all_completed(&blck)
      @mut.synchronize do
        ensure_not_started!
        @all_completed_blocks << blck
      end
    end

    def init(job_id, arg={})
      log "Lifecycle Hooks Init"
      @mut.synchronize do
        @started = true
        @started_blocks.each do |b|
          b.call(job_id, arg)
        end
      end
    end

    def completed(job_id, arg={})
      log "Lifecycle Hooks Completed"
      @mut.synchronize do
        @completed_blocks.each do |b|
          b.call(job_id, arg)
        end
      end
    end

    def all_completed(silent=false)
      log "Lifecycle Hooks All Completed"
      @mut.synchronize do
        @all_completed_blocks.each do |b|
          b.call(silent)
        end
      end
    end

    def killed(job_id, arg={})
      @mut.synchronize do
        @killed_blocks.each do |b|
          b.call(job_id, arg)
        end
      end
    end

    def exit(job_id, arg={})
      @mut.synchronize do
        @exited_blocks.each do |b|
          b.call(job_id, arg)
        end
      end
    end
  end
end
