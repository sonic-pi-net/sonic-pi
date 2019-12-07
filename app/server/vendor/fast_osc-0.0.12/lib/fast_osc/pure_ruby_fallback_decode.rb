#--
# This file was part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

module SonicPi
  module OSC
    class OscDecode

      def initialize(use_cache = false, cache_size=1000)
        @float_cache = {}
        @integer_cache = {}
        @cache_size = cache_size

        @num_cached_integers = 0
        @num_cached_floats = 0
        @string_terminator = "\x00".freeze
        @args = []
        @i_tag = "i".freeze
        @f_tag = "f".freeze
        @s_tag = "s".freeze
        @d_tag = "d".freeze
        @h_tag = "h".freeze
        @b_tag = "b".freeze

        @cap_n = 'N'.freeze
        @cap_g = 'G'.freeze
        @low_g = 'g'.freeze
        @q_lt = 'q>'.freeze
        @binary_encoding = "BINARY".freeze
      end

      def decode_single_message(m)
        ## Note everything is inlined here for effienciency to remove the
        ## cost of method dispatch. Apologies if this makes it harder to
        ## read & understand. See http://opensoundcontrol.org for spec.
        m.force_encoding(@binary_encoding)

        args, idx = [], 0

        # Get OSC address e.g. /foo
        orig_idx = idx
        idx = m.index(@string_terminator, orig_idx)
        address, idx =  m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)

        sep, idx = m[idx], idx + 1

        # Let's see if we have some args..
        if sep == ?,

          # Get type tags
          orig_idx = idx
          idx = m.index(@string_terminator, orig_idx)
          tags, idx = m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)

          tags.each_char do |t|
            case t
            when @i_tag
              # int32
              raw = m[idx, 4]
              arg, idx = @integer_cache[raw], idx + 4

              unless arg
                arg = raw.unpack(@cap_n)[0]
                # Values placed inline for efficiency:
                # 2**32 == 4294967296
                # 2**31 - 1 == 2147483647
                arg -= 4294967296 if arg > 2147483647
                if @num_cached_integers < @cache_size
                  @integer_cache[raw] = arg
                  @num_cached_integers += 1
                end
              end
            when @f_tag
              # float32
              raw = m[idx, 4]
              arg, idx = @float_cache[raw], idx + 4
              unless arg
                arg = raw.unpack(@low_g)[0]
                if @num_cached_floats < @cache_size
                  @float_cache[raw] = arg
                  @num_cached_floats += 1
                end
              end
            when @s_tag
              # string
              orig_idx = idx
              idx = m.index(@string_terminator, orig_idx)
              arg, idx =  m[orig_idx...idx], idx + 1 + ((4 - ((idx + 1) % 4)) % 4)
            when @d_tag
              # double64
              arg, idx = m[idx, 8].unpack(@cap_g)[0], idx + 8
            when @h_tag
              # int64
              arg, idx = m[idx, 8].unpack(@q_lt)[0], idx + 8
            when @b_tag
              # binary blob
              l = m[idx, 4].unpack(@cap_n)[0]
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
end

module FastOsc
  warn "Overriding fast_osc c-extension FastOsc::decode_single_message, falling back to pure Ruby version"
  def self.decode_single_message(m)
    SonicPi::OSC::OscDecode.new.decode_single_message(m)
  end
end
