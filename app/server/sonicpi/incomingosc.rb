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
  class IncomingOSC
    attr_reader :address, :args
    def initialize(s)
      p = OSC::NetworkPacket.new(s)
      @address = get_string(p)
      @args = get_args(p)
    end

    private

    def get_string(p)
      string_delemeter = "\x00"
      result = ''
      until (c = p.getc) == string_delemeter
	      result << c
      end
      p.skip_padding
      result
    end

    def get_args(p)
      if p.getc == ?,

        tags = get_string(p)
        args = []

        tags.scan(/./) do | tag |
          args << case tag
                  when "i"
                    get_int32(p)
                  when "f"
                    get_float32(p)
                  when "s"
                    get_string(p)
                  when "d"
                    get_double64(p)
                  when "b"
                    get_blob(p)
                  else
                    raise "unknown OSC tag: #{tag}"
                  end
        end
        args
      else
        []
      end
    end

    def get_int32(p)
      i = p.getn(4).unpack('N')[0]
      i -= 2**32 if i > (2**31-1)
      p.skip_padding
      i
    end

    def get_float32(p)
      f = p.getn(4).unpack('g')[0]
      p.skip_padding
      f
    end

    def get_blob(p)
      l = p.getn(4).unpack('N')[0]
      b = p.getn(l)
      p.skip_padding
      b
    end

    def get_double64(p)
      f = p.getn(8).unpack('G')[0]
      p.skip_padding
      f
    end

  end
end
