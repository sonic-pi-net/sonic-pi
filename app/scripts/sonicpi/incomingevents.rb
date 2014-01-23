require 'thread'
require_relative "util"

module SonicPi
  class IncomingEvents

    def initialize
      @gensym_id_sem = Mutex.new
      @current_gensym_id = 0
      @handler_sem = Mutex.new
      @handlers = {}
    end

    def gensym(s)
      @gensym_id_sem.synchronize do
        id = @current_gensym_id += 1
        "#{s}-#{id}"
      end
    end

    def event(handle, payload)
      @handler_sem.synchronize do
        hs = @handlers[handle] || {}
        hs.each do |key, fn|
          res = fn.call payload
          if(res == :remove_handler)
            hs.delete key
          end
        end
      end
    end

    def add_handler(handle, key, &block)
      @handler_sem.synchronize do
        @handlers[handle] = {} unless @handlers[handle]
        @handlers[handle][key] = block
      end
    end

    def oneshot_handler(handle, &block)
      add_handler(handle, gensym("sonicpi/incomingevents/oneshot")) do |payload|
        block.call payload
        :remove_handler
      end
    end

    def rm_handler(handle, key)
      @handler_sem.synchronize do
        if @handlers[handle]
          @handlers[handle].delete key
        end
      end
    end
  end
end
