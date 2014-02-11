require "hamster/hash"
require_relative "util"
require_relative "atom"


module SonicPi
  class IncomingEvents

    def initialize
      @current_gensym_id_A = Atom.new(0)
      @handlers_A = Atom.new(Hamster.hash)
    end

    def gensym(s)
      id = @current_gensym_id_A.swap!{|el| el + 1}
      "#{s}-#{id}"
    end

    def event(handle, payload)
      handlers = @handlers_A.deref
      hs = handlers[handle] || {}

      hs.each do |key, fn|
        res = fn.call payload
        if(res == :remove_handler)
          rm_handler(handle, key)
        elsif (res.kind_of?(Array) && (res.size == 2) && (res.first == :remove_handlers))
          res[1].each do |h_info|
            rm_handler(h_info[0], h_info[1])
          end
        end
      end
    end

    def add_handler(handle, key, &block)
      @handlers_A.swap! do |hs|
        handlers = hs[handle] || Hamster.hash
        hs.put(handle, handlers.put(key, block))
      end
    end

    def oneshot_handler(handle, &block)
      add_handler(handle, gensym("sonicpi/incomingevents/oneshot")) do |payload|
        block.call payload
        :remove_handler
      end
    end

    def rm_handler(handle, key)
      @handlers_A.swap! do |hs|
        handlers = hs[handle] || Hamster.hash
        hs.put(handle, handlers.delete(key))
      end
    end
  end
end
