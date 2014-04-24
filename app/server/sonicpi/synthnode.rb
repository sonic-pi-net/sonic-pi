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
require File.absolute_path("#{File.dirname(__FILE__)}/node")
require 'active_support/core_ext/hash/indifferent_access'

module SonicPi
  class SynthNode < Node

    attr_reader :name, :args

    def initialize(id, comms, name, args, arg_validation_fn = nil)
      super(id, comms, arg_validation_fn)
      @args = args.with_indifferent_access
      @name = name
      @control_mutex = Mutex.new
    end

    def ctl(*args)
      @control_mutex.synchronize do
        a_h = Hash[*args]
        @args = @args.merge(a_h)
        super
      end
    end

    def control(*args)
      ctl(*args)
    end

    def to_s
      "#<SonicPi::SynthNode @id=#{@id}, @name=#{@name}>"
    end
  end
end
