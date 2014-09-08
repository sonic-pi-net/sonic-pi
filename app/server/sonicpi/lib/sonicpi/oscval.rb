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
require 'osc-ruby'

module SonicPi
  class OSCVal
    def initialize(port)
      server = OSC::Server.new(port, true)
      @vals = {}
      server.add_method '*' do |m|
        @vals[m.address] = m.to_a || []
      end

      @server_thread = Thread.new do
        Thread.current.thread_variable_set(:sonic_pi_thread_group, :osc_val_server)
        server.run
      end
    end

    def read(path, idx=0)
      (@vals[path] || [])[idx] || 0
    end

    def read_all(path)
      @vals[path] || []
    end

  end
end
