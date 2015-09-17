#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  class OscDecode

    def initialize(use_cache = false, cache_size=1000)
      @float_cache = {}
      @integer_cache = {}
      @cache_size = cache_size

      @num_cached_integers = 0
      @num_cached_floats = 0
    end

    def decode_single_message(m)
      ## Note everything is inlined here for effienciency to remove the
      ## cost of method dispatch. Apologies if this makes it harder to
      ## read & understand. See http://opensoundcontrol.org for spec.

      m.force_encoding("BINARY")

      args, idx, string_terminator = [], 0, "\x00"

      # Get OSC address e.g. /foo
      orig_idx = idx
      idx = m.index(string_terminator, orig_idx)
      address, idx =  m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)

      sep, idx = m[idx], idx + 1

      # Let's see if we have some args..
      if sep == ?,

        # Get type tags
        orig_idx = idx
        idx = m.index(string_terminator, orig_idx)
        tags, idx = m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)

        tags.each_char do |t|
          case t
          when "i"
            # int32
            raw = m[idx, 4]
            arg, idx = @integer_cache[raw], idx + 4

            unless arg
              arg = raw.unpack('N')[0]
              # Values placed inline for efficiency:
              # 2**32 == 4294967296
              # 2**31 - 1 == 2147483647
              arg -= 4294967296 if arg > 2147483647
              if @num_cached_integers < @cache_size
                @integer_cache[raw] = arg
                @num_cached_integers += 1
              end
            end
          when "f"
            # float32
            raw = m[idx, 4]
            arg, idx = @float_cache[raw], idx + 4
            unless arg
              arg = raw.unpack('g')[0]
              if @num_cached_floats < @cache_size
                @float_cache[raw] = arg
                @num_cached_floats += 1
              end
            end
          when "s"
            # string
            orig_idx = idx
            idx = m.index(string_terminator, orig_idx)
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
