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
require_relative "group"

module SonicPi
  class ChordGroup < Group

    def initialize(group)
      @sub_nodes = []
      super(group.id, group.comms)
      @sem = Mutex.new
    end

    def sub_nodes=(nodes)
      raise "Sub nodes already registered!" unless @sub_nodes.empty?
      @sub_nodes = nodes
      should_kill_group = lambda do
        @sem.synchronize do
          if @sub_nodes.all?{|sn| sn.destroyed?}
            self.kill
          end
        end
      end

      @sub_nodes.each {|sn| sn.on_destroyed(&should_kill_group)}
      should_kill_group.call
      nodes
    end

    def nodes
      raise "Sub nodes not registered!" if @sub_nodes.empty?
      @sub_nodes
    end

    def sub_nodes
      nodes
    end

    def to_s
      "#<SonicPi::ChordGroup @id=#{@id} @subnodes=#{@sub_nodes}>"
    end
  end
end
