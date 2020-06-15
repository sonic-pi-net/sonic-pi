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
require_relative "group"

module SonicPi
  class ChordGroup < Group

    attr_reader :notes

    def initialize(group, notes, info)
      @notes = notes
      @sub_nodes = []
      super(group.id, group.comms)
      @sem = Mutex.new
      @info = info
    end

    def control(*args)
      args_h = resolve_synth_opts_hash_or_array(args)

      notes = args_h[:notes]
      if notes && notes.respond_to?(:each_with_index)
        # We're trying to change all the notes of this
        # chord group to a different set of notes
        a = args_h[:amp]
        s = @sub_nodes.size
        args_h[:amp] = a.to_f / s if a && s > 0
        args_h.delete(:notes)
        r_notes = notes.ring
        @sub_nodes.each_with_index do |sn, idx|
          sn.control(args_h.merge({:note => r_notes[idx]}))
        end
      else
        super
      end
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
