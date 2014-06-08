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
require "hamster/hash"
require_relative "util"
require_relative "atom"


module SonicPi
  class IncomingEvents

    def initialize
      @handlers_A = Atom.new(Hamster.hash)
    end

    def gensym(s)
      "#{s}-#{rand}"
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
