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

module SonicPi
  class OscDecode

    def initialize
    end

    def decode_single_message(m)
      ## Note everything is inlined here for effienciency to remove the
      ## cost of method dispatch. Apologies if this makes it harder to
      ## read & understand. See http://opensoundcontrol.org for spec.

      m.force_encoding("BINARY")

      args = []
      idx = 0
      string_terminator = "\x00"

      # Get OSC address e.g. /foo
      orig_idx = idx
      idx += 1 while m[idx] != string_terminator
      address, idx =  m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)

      sep, idx = m[idx], idx + 1

      # Let's see if we have some args..
      if sep == ?,

        # Get type tags
        orig_idx = idx
        idx += 1 while m[idx] != string_terminator
        tags, idx = m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)

        tags.each_char do |t|
          case t
          when "i"
            # int32
            i = m[idx, 4].unpack('N')[0]

            # Values placed inline for efficiency:
            # 2**32 == 4294967296
            # 2**31 - 1 == 2147483647
            i -= 4294967296 if i > 2147483647
            arg, idx = i, idx + 4
          when "f"
            # float32
            arg, idx = m[idx, 4].unpack('g')[0], idx + 4
          when "s"
            # string
            orig_idx = idx
            idx += 1 while m[idx] != string_terminator
            arg, idx =  m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)
          when "d"
            # double64
            arg, idx = m[idx, 8].unpack('G')[0], idx + 8
          when "h"
            # int64
            arg, idx = m[idx, 8].unpack('q>')[0], idx + 8
          when "b"
            # binary blob
            l = m[idx, 4].unpack('N')[0]
            idx += 4
            arg = m[idx, l]
            idx += l
            #Skip Padding
            idx += ((4 - (idx % 4)) % 4)
          else
            raise "Unknown OSC type #{t}"
          end

          args << arg
        end
      end
      return address, args
    end

  end
end
