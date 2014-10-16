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
  class OscEncode
    def initialize
      @string_cache = {}
      @bundle_header = get_from_or_add_to_string_cache("#bundle")
    end

    def encode_single_message(address, *args)
      message = ""
      address = get_from_or_add_to_string_cache(address)
      tags = ","
      args_encoded = ""

      args.each { |a| append_encoded_arg!(tags, args_encoded, a) }

      tags_encoded = get_from_or_add_to_string_cache(tags)

      message << address << tags_encoded << args_encoded
    end

    def encode_single_bundle(ts, address, *args)
      message = encode_single_message(address, *args)
      message_encoded = [message.size].pack('N') << message

      bundle = ""
      bundle << @bundle_header << time_encoded(ts) << message_encoded
    end

    private

    def append_encoded_arg!(tags, args_encoded, arg)
      case arg
      when Integer
        tags << 'i'
        args_encoded << [arg].pack('N').force_encoding("BINARY")
      when Float
        tags << 'f'
        args_encoded << [arg].pack('g').force_encoding("BINARY")
      when String
        tags << 's'
        args_encoded << get_from_or_add_to_string_cache(arg)
      else
        raise "Unknown arg type to encode: #{arg}"
      end
    end

    def get_from_or_add_to_string_cache(s)
      if cached = @string_cache[s]
        return cached
      else
        res = padding(s.sub(/\000.*\z/, '') + "\000").force_encoding("BINARY")
        @string_cache[s] = res
        return res
      end
    end

    def padding(s)
      s + ("\000" * ((4 - (s.size % 4)) % 4))
    end

    def time_encoded(time)
      ntp = time.to_f + 2208988800
      t1, fr = ntp.divmod(1)
      # 2 ** 32 == 4294967296
      t2 = (fr * 4294967296).to_i
      [t1, t2].pack('N2')
    end
  end
end
