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
require 'thread'

module SonicPi
  class Node
    include SonicPi::Util

    attr_reader :id, :comms, :info

    def initialize(id, comms, info=nil)
      @id = id
      @comms = comms
      @state = :pending
      @state_change_sem = Mutex.new
      @on_destroyed_callbacks = []
      @on_started_callbacks = []
      @on_move_callbacks = Queue.new
      @info = info
      r = rand.to_s
      @killed_event_key  = "/sonicpi/node/killed#{id}-#{r}"
      @paused_event_key  = "/sonicpi/node/paused#{id}-#{r}"
      @started_event_key = "/sonicpi/node/started#{id}-#{r}"
      @created_event_key = "/sonicpi/node/created#{id}-#{r}"
      @moved_event_key = "/sonicpi/node/moved#{id}-#{r}"

      @comms.async_add_event_handlers(["/n_end/#{id}", @killed_event_key,  method(:handle_n_end)],
                                      ["/n_on/#{id}",  @started_event_key, method(:handle_n_on)],
                                      ["/n_go/#{id}",  @created_event_key, method(:handle_n_go)],
                                      ["/n_move/#{id}",@moved_event_key,   method(:handle_n_move)],
                                      ["/n_off/#{id}", @paused_event_key, method(:handle_n_off)])

    end


    # block will be called when the node is destroyed or immediately if
    # node is already destroyed. Possibly executed on a separate thread.
    def on_destroyed(&block)
      @state_change_sem.synchronize do
        if @state == :destroyed
          block.call
        else
          @on_destroyed_callbacks << block
        end
      end
    end

    def on_started(&block)
      @state_change_sem.synchronize do
        if @state != :pending
          block.call
        else
          @on_started_callbacks << block
        end
      end
    end

    def on_move(&block)
      @on_move_callbacks << block
    end

    def wait_until_started(timeout=nil)
      prom = nil
      @state_change_sem.synchronize do
        if @state != :pending
          return self
        else
          prom = Promise.new
          cb = lambda do
            prom.deliver! :stop_waiting_for_node
          end
          @on_started_callbacks << cb
        end
      end
      prom.get(timeout)
      self
    end

    def move(new_group, pos=nil, now=false, &blk)
      if blk
        key = "/sonicpi/node/moved/#{self.id}/#{rand.to_s}"
        @comms.add_event_handler("/n_move/#{id}", key) do |payload|
          _, target_node = *payload
          if target_node.to_i == new_group.to_i
            blk.call
            :remove_handler
          end
        end
      end
      @comms.node_move(self, new_group, pos, now)
      self
    end

    def kill(now=false)
      @comms.kill_node self, now
      self
    end

    def pause(now=false)
      @comms.node_pause self, now
      self
    end

    def run(now=false)
      @comms.node_run self, now
      self
    end

    def ctl(*args)
      args_h = resolve_synth_opts_hash_or_array(args)
      @comms.node_ctl self, args_h
      self
    end

    def ctl_now(*args)
      args_h = resolve_synth_opts_hash_or_array(args)
      @comms.node_ctl self, args_h, true
      self
    end

    def control(*args)
      ctl(*args)
    end

    def live?
      !(@state == :destroyed)
    end

    def destroyed?
      @state == :destroyed
    end

    def paused?
      @state == :paused
    end

    def running?
      @state == :running
    end

    def state
      @state
    end

    def to_i
      @id.to_i
    end

    def to_f
      @id.to_f
    end

    def to_s
      "#<SonicPi::Node @id=#{@id}>"
    end

    def inspect
      to_s
    end

    def blank_node?
      false
    end

    def sp_thread_safe?
      true
    end


    private

    def call_on_destroyed_callbacks
      begin
        @on_destroyed_callbacks.each{|cb| cb.call}
        @on_destroyed_callbacks = []
      rescue Exception => e
        log_exception e, "in on destroyed callbacks"
      end
    end

    def call_on_started_callbacks
      begin
        @on_started_callbacks.each{|cb| cb.call}
        @on_started_callbacks = []
      rescue Exception => e
        log_exception e, "in on started callbacks"
      end
    end

    def call_on_move_callbacks(arg)
      begin

        @on_move_callbacks.length.times do
          cb = @on_move_callbacks.pop
          if cb.arity == 0
            cb.call
          else
            cb.call(arg)
          end
        end
      rescue Exception => e
        log_exception e, "in on move callbacks"
      end
    end


    def handle_n_off(arg)
      @state_change_sem.synchronize do
        @state = :paused
      end
      nil
    end

    def handle_n_on(arg)
      @state_change_sem.synchronize do
        @state = :running
      end
      nil
    end

    def handle_n_move(arg)
      @state_change_sem.synchronize do
        call_on_move_callbacks(arg)
      end
      nil
    end

    def handle_n_go(arg)
      @state_change_sem.synchronize do
        prev_state = @state
        @state = :running
        call_on_started_callbacks if prev_state == :pending
      end
      nil
    end

    def handle_n_end(arg)
        @state_change_sem.synchronize do
          prev_state = @state
          @state = :destroyed
          call_on_destroyed_callbacks if prev_state != :destroyed
        end
        [:remove_handlers,
          [ ["/n_go/#{@id}",  @created_event_key],
            ["/n_off/#{@id}", @paused_event_key],
            ["/n_on/#{@id}",  @started_event_key],
            ["/n_move/#{@id}",@moved_event_key],
            ["/n_end/#{@id}", @killed_event_key]]]

    end
  end
end
