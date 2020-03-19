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
        @bundle_header = "#bundle\x00".freeze
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

        @literal_magic_time_offset = 2208988800
        @literal_cap_n = 'N'.freeze
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
              arg.force_encoding("UTF-8")
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

      def decode(m, output=[])
        tt = parse_timetag(m)
        raw_msgs, raw_bundles = parse_bundles(m)

        output << [tt, raw_msgs.map {|m| self.decode_single_message(m) }]

        if raw_bundles.any?
          output << raw_bundles[1..-1]
          self.decode(raw_bundles.first, output)
        else
          # for compatibility with the C Extension API
          return output.reject {|msg| msg.empty? }.reverse
        end
      end

      private

      def parse_timetag(m)
        return nil unless is_bundle?(m)

        t1 = m[8..11].unpack(@literal_cap_n).first
        t1 = (t1 - @literal_magic_time_offset)

        t2 = m[12..15].unpack(@literal_cap_n).first
        Time.at(t1 + t2)
      end

      def parse_bundles(m)
        raw_msgs = []
        raw_bundles = []

        if is_bundle?(m)
          rest_of_message = m[16..-1]

          while rest_of_message && rest_of_message.bytesize > 0
            first_element_length = rest_of_message[0..3].unpack(@literal_cap_n).first
            first_element = rest_of_message[4..(4 + first_element_length - 1)]

            if is_bundle?(first_element)
              raw_bundles << first_element
            else
              raw_msgs << first_element
            end

            rest_of_message = rest_of_message[(4 + first_element_length)..-1]
          end
        else
          raw_msgs << m
        end

        [raw_msgs, raw_bundles]
      end

      def is_bundle?(m)
        m[0..7] == @bundle_header
      end
    end
  end
end
