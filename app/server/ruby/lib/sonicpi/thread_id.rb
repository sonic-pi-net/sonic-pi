# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2017 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++


module SonicPi

  class ThreadId
    include Comparable

    attr_reader :ids

    def self.new_from_serialized(str)
      _, name, vals = str.split('::-::')
      raise "Unable to deserialize ThreadId: #{str}" unless name == "thread-id"
      ids = vals.split('_').map{|v| v.to_i}
      new(*ids)
    end

    def initialize(*ids)
      @ids = ids.map { |i| i.to_i }.freeze
      @serialized = "::-::thread-id::-::#{ids.join('_')}"
    end

    def <<(other)
      self.class.new(*(@ids + [other]))
    end

    def ==(other)
      @ids == other.ids
    end

    def <=>(other)
      @ids.each_with_index do |el, idx|
        # self is larger than other
        return 1 if idx >= other.ids.size
        return -1 if el < other.ids[idx]
        return 1 if el > other.ids[idx]
      end

      # other is larger than self
      return -1 if other.ids.size > @ids.size

      # they are both of similar size
      # with similar elements
      return 0
    end

    def sp_thread_safe?
      true
    end

    def __sp_make_thread_safe
      self
    end

    def to_s
      "#<SonicPi::ThreadId #{@ids.inspect}>"
    end

    def serialize
      @serialized
    end

    def inspect
      to_s
    end
  end
end
