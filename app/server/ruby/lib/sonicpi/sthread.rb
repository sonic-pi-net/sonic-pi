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
require_relative "counter"

module SonicPi
  class SThread

    @@counter = Counter.new
    attr_reader :name, :job_id, :thread, :id
    def initialize(name, job_id, t)
      @id = @@counter.next
      @name = name
      @job_id = job_id
      @thread = t
    end
  end
end
