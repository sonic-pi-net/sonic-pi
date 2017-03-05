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

require_relative "util"
require_relative "counter"
require_relative "promise"

module SonicPi
  class IncomingEvents
    include Util

    def initialize(name=:event_handler, priority=0)
      @event_queue = SizedQueue.new(50)
      @handlers = {}
      @continue = true
      @handler_thread = Thread.new do
        __system_thread_locals.set_local(:sonic_pi_local_thread_group, name)
        Thread.current.priority = priority
        while @continue do
          consume_event
        end
      end
    end

    def gensym(s)
      "#{s}-#{rand}"
    end

    def event(handle, payload)
      prom = Promise.new
      @event_queue << [:event, [handle, payload, prom]]
      prom.get(5)
    end

    def async_event(handle, payload)
      @event_queue << [:async_event, [handle, payload]]
    end

    def add_handler(handle, key, &block)
      prom = Promise.new
      @event_queue << [:add, [handle, key, block, prom]]
      prom.get(5)
    end

    def async_add_handlers(*args)
      @event_queue << [:async_add_multiple, args]
    end

    def async_add_handler(handle, key, &block)
      @event_queue << [:async_add, [handle, key, block]]
    end

    def oneshot_handler(handle, &block)
      add_handler(handle, gensym("sonicpi/incomingevents/oneshot")) do |payload|
        block.call payload
        :remove_handler
      end
    end

    def async_oneshot_handler(handle, &block)
      async_add_handler(handle, gensym("sonicpi/incomingevents/oneshot")) do |payload|
        block.call payload
        :remove_handler
      end
    end

    def async_multi_oneshot_handler(handles, &block)
      return async_oneshot_handler(handles[0], &block) if handles.size == 1

      key_prefix = gensym("sonicpi/incomingevents/oneshot")

      handles_keys = handles.map {|h| [h, "#{key_prefix}-#{h}"]}
      handles_keys.each do |h, k|
        async_add_handler(h, k) do |payload|
          block.call payload
          handles_keys.each { |han, key| q_rm_handler(han, key) }
        end
      end
    end

    def rm_handler(handle, key)
      prom = Promise.new
      @event_queue << [:rm, [handle, key, prom]]
      prom.get(5)
    end

    def reset!
      @event_queue.clear
      @event_queue << [:reset, []]
    end

    def shutdown
      @event_queue << [:quit, []]
    end

    def to_s
      "Incoming events: #{@handlers.inspect}, #{@event_queue.inspect}"
    end

    def inspect
      to_s
    end

    private

    def q_sync_insert_handler(handle, key, block, prom)
      q_insert_handler(handle, key, block)
      prom.deliver! true
    end

    def q_insert_handler(handle, key, block)
      if keyed_handlers = @handlers[handle]
        keyed_handlers[key] = block
      else
        @handlers[handle] = {key => block}
      end
    end

    def q_handle_sync_event(handle, payload, prom)
      q_handle_event(handle, payload)
      prom.deliver! :incoming_events_sync
    end

    def q_handle_event(handle, payload)
      if hs = @handlers[handle]
        hs.each do |key, fn|
          begin
            res = fn.call payload
            if res
              if(res == :remove_handler)
                q_rm_handler handle, key
              elsif (res.kind_of?(Array) && (res.size == 2) && (res.first == :remove_handlers))
                res[1].each do |h_info|
                  q_rm_handler(h_info[0], h_info[1])
                end
              end
            end
          rescue Exception => e
            log_exception e
          end
        end

      end
    end

    def q_rm_handler(handle, key)
      if keyed_hs = @handlers[handle]
        keyed_hs.delete key
        @handlers.delete(handle) if keyed_hs.empty?
      end
    end

    def q_sync_rm_handler(handle, key, prom)
      q_rm_handler(handle, key)
      prom.deliver! true
    end


    def consume_event
      action, content = @event_queue.pop
      case action
      when :async_add_multiple
        content.each do |c|
          q_insert_handler(*c)
        end
      when :async_event
        q_handle_event(*content)
      when :async_add
        q_insert_handler(*content)
      when :event
        q_handle_sync_event(*content)
      when :add
        q_sync_insert_handler(*content)
      when :rm
        q_sync_rm_handler(*content)
      when :quit
        @continue = false
      when :reset
        @event_queue.clear
        @handlers = {}
      end
    end
  end
end
