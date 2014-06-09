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
require_relative "node"

module SonicPi
  class Group < Node

    def initialize(id, comms)
      super(id, comms)
      @nodes = {}
      @pending_nodes = {}
      @pending_nodes_mut = Mutex.new
      @nodes_mut = Mutex.new


      @on_destroyed_callbacks << lambda do
        @pending_nodes.values.each do |pn|
          # These nodes have been triggered from Sonic Pi, but the
          # containing group must have been killed on the server before
          # the messages managed to get out. Emulate the server's node
          # end OSC message.

          # It's therefore possible that multiple of these messages
          # could be sent - however, this won't cause any issues as the
          # node won't run on_destroyed handlers multiple times and the
          # default /n_end handlers remove themselves.
          @comms.async_event "/n_end/#{pn.id}", {}
        end
      end
    end

    def subnode_add(n)
      @nodes_mut.synchronize do
        @nodes[n.id] = n
      end

      @pending_nodes_mut.synchronize do
        @pending_nodes[n.id] = n
      end

      n.on_started do
        @pending_nodes_mut.synchronize do
          @pending_nodes.delete(n.id)
        end
      end
    end

    def subnode_rm(n)
      @nodes_mut.synchronize do
        @nodes.delete n.id
      end
    end

    def clear
      @comms.group_clear @id
      self
    end

    def deep_clear
      @comms.group_deep_clear @id
      self
    end

    def to_s
      "#<SonicPi::Group @id=#{@id}>"
    end
  end

end
