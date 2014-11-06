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
    def initialize(use_cache = false, cache_size=1000)
      @use_cache = use_cache
      @integer_cache = {}
      @string_cache = {}
      @float_cache = {}
      @cache_size = cache_size

      @num_cached_integers = 0
      @num_cached_floats = 0
      @num_cached_strings = 0

      @bundle_header = get_from_or_add_to_string_cache("#bundle")
    end

    def encode_single_message(address, args=[])
      message = ""
      address = get_from_or_add_to_string_cache(address)
      tags = ","
      args_encoded = ""

      args.each { |a| append_encoded_arg!(tags, args_encoded, a) }

      tags_encoded = get_from_or_add_to_string_cache(tags)

      message << address << tags_encoded << args_encoded
    end

    def encode_single_bundle(ts, address, args=[])
      message = encode_single_message(address, args)
      message_encoded = [message.size].pack('N') << message

      bundle = ""
      bundle << @bundle_header << time_encoded(ts) << message_encoded
    end

    private

    def append_encoded_arg!(tags, args_encoded, arg)
      case arg
      when Integer
        tags << 'i'
        if @use_cache
          args_encoded << get_from_or_add_to_integer_cache(arg)
        else
          args_encoded << encode_integer(arg)
        end
      when Float
        tags << 'f'
        if @use_cache
          args_encoded << get_from_or_add_to_float_cache(arg)
        else
          args_encoded << encode_float(arg)
        end
      when String, Symbol
        arg = arg.to_s
        tags << 's'
        if @use_cache
          args_encoded << get_from_or_add_to_string_cache(arg)
        else
          args_encoded << encode_string(arg)
        end
      else
        raise "Unknown arg type to encode: #{arg}"
      end
    end

    def get_from_or_add_to_float_cache(f)
      if cached = @float_cache[f]
        return cached
      else
        res = encode_float(f)
        if @num_cached_floats < @cache_size
          # only cache the first @max_cached_floats floats
          # to avoid a massive memory leak as floats are
          # often randomly generated
          @float_cache[f] = res
          @num_cached_floats += 1
          # log "caching float #{f}"
        end
        return res
      end
    end

    def get_from_or_add_to_string_cache(s)
      if cached = @string_cache[s]
        return cached
      else
        res = encode_string(s)
        if @num_cached_strings < @cache_size
          # only cache the first @cache_size strings to avoid a memory
          # memory leak.
          @string_cache[s] = res
          @num_cached_strings += 1
          # log "caching string #{s}"
        end
        return res
      end
    end

    def get_from_or_add_to_integer_cache(i)
      if cached = @integer_cache[i]
        return cached
      else
        res = encode_integer(i)
        if @num_cached_integers < @cache_size
          # only cache the first @cache_size integers to avoid a memory
          # memory leak.
          @integer_cache[i] = res
          @num_cached_integers += 1
          # log "caching integer #{i}"
        end
        return res
      end

    end

    def encode_float(f)
      [f].pack('g').force_encoding("BINARY")
    end

    def encode_integer(i)
      [i].pack('N').force_encoding("BINARY")
    end

    def encode_string(s)
      padding(s.sub(/\000.*\z/, '') + "\000").force_encoding("BINARY")
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
