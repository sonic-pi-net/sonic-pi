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
  class OscEncode
    # Apologies for the density of this code - I've inlined a lot of the
    # code to reduce method dispatch overhead and to increase efficiency.
    # See http://opensoundcontrol.org for spec.

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
      message, args_encoded, tags = "", "", ","
      message.force_encoding("BINARY")
      address = get_from_or_add_to_string_cache(address)

      args.each do |arg|
        case arg
        when Integer
          tags << 'i'

          if @use_cache
            if cached = @integer_cache[arg]
              args_encoded << cached
            else
              res = [arg].pack('N')
              if @num_cached_integers < @cache_size
                @integer_cache[arg] = res
                @num_cached_integers += 1
                # log "caching integer #{arg}"
              end
              args_encoded << res
            end
          else
            args_encoded << [arg].pack('N')
          end
        when Float, Rational
          arg = arg.to_f
          tags << 'f'
          if @use_cache
            if cached = @float_cache[arg]
              args_encoded << cached
            else
              res = [arg].pack('g')
              if @num_cached_floats < @cache_size
                @float_cache[arg] = res
                @num_cached_floats += 1
                # log "caching float #{arg}"
              end
              args_encoded << res
            end
          else
            args_encoded << [arg].pack('g')
          end
        when String, Symbol
          arg = arg.to_s
          tags << 's'
          if @use_cache
            if cached = @string_cache[arg]
              args_encoded << cached
            else
              res = encode_string(arg)
              if @num_cached_strings < @cache_size
                @string_cache[arg] = res
                @num_cached_strings += 1
                # log "caching string #{arg}"
              end
              args_encoded << res
            end
          else
            args_encoded << encode_string(arg)
          end
        else
          raise "Unknown arg type to encode: #{arg}"
        end
      end

      tags_encoded = get_from_or_add_to_string_cache(tags)
      message << address << tags_encoded << args_encoded
    end

    def encode_single_bundle(ts, address, args=[])
      message = encode_single_message(address, args)
      message_encoded = [message.size].pack('N') << message
        "" << @bundle_header << time_encoded(ts) << message_encoded
    end

    private

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
        return  res
      end
    end

    def encode_string(s)
      s = s.sub(/\000.*\z/, '')
      s << "\000"
      (s << ("\000" * ((4 - (s.bytesize % 4)) % 4)))
    end

    def time_encoded(time)
      t1, fr = (time.to_f + 2208988800).divmod(1)
      # 2 ** 32 == 4294967296
      t2 = (fr * 4294967296).to_i
      [t1, t2].pack('N2')
    end
  end

  class StreamOscEncode < OscEncode
    def encode_single_message(address, args=[])
      message = super
      ([message.length].pack('N') << message).force_encoding("BINARY")
    end

    def encode_single_bundle(ts, address, args=[])
      message = super
      message.count.pack('N') << message
    end
  end
end
