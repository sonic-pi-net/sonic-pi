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
  class Node
    attr_reader :id, :comms

    def initialize(id, comms, info=nil)
      @id = id
      @comms = comms
      @state = :pending
      @state_change_sem = Mutex.new
      @on_destroyed_callbacks = []
      @on_started_callbacks = []
      @info = info
      r = rand.to_s
      @killed_event_key  = "/sonicpi/node/killed#{id}-#{r}"
      @paused_event_key  = "/sonicpi/node/paused#{id}-#{r}"
      @started_event_key = "/sonicpi/node/started#{id}-#{r}"
      @created_event_key = "/sonicpi/node/created#{id}-#{r}"

      @comms.async_add_event_handlers(["/n_end/#{id}", @killed_event_key,  method(:handle_n_end)],
                                      ["/n_on/#{id}",  @started_event_key, method(:handle_n_on)],
                                      ["/n_go/#{id}",  @created_event_key, method(:handle_n_go)],
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

    def wait_until_started
      prom = nil
      @state_change_sem.synchronize do
        if @state != :pending
          return true
        else
          prom = Promise.new
          cb = lambda do
            prom.deliver! :stop_waiting_for_node
          end
          @on_started_callbacks << cb
        end
      end
      prom.get
      self
    end

    def kill(now=false)
      @comms.kill_node @id, now
      self
    end

    def pause(now=false)
      @comms.node_pause @id, now
      self
    end

    def run(now=false)
      @comms.node_run @id, now
      self
    end

    def ctl(*args)
      args_h = resolve_synth_opts_hash_or_array(args)
      if Thread.current.thread_variable_get(:sonic_pi_mod_sound_check_synth_args)
        @info.validate!(args_h) if @info
      end
      @comms.node_ctl @id, args_h
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

    private

    def call_on_destroyed_callbacks
      begin
        @on_destroyed_callbacks.each{|cb| cb.call}
        @on_destroyed_callbacks = []
      rescue Exception => e
        Kernel.puts "Exception in on destroyed callbacks: #{e.message}"
        e.backtrace.each do |b|
          Kernel.puts b
        end
      end
    end

    def call_on_started_callbacks
      begin
        @on_started_callbacks.each{|cb| cb.call}
        @on_started_callbacks = []
      rescue Exception => e
        Kernel.puts "Exception in on started callbacks: #{e.message}"
        e.backtrace.each do |b|
          Kernel.puts b
        end
      end
    end

    def handle_n_off(arg)
      @state_change_sem.synchronize do
        @state = :paused
      end
    end

    def handle_n_on(arg)
      @state_change_sem.synchronize do
        @state = :running
      end
    end

    def handle_n_go(arg)
      @state_change_sem.synchronize do
        prev_state = @state
        @state = :running
        call_on_started_callbacks if prev_state == :pending
      end
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
            ["/n_end/#{@id}", @killed_event_key]]]

    end
  end
end
