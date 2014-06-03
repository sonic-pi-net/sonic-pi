#
# Copyright (C) 2009, 2010 Wayne Meissner
# Copyright (C) 2009 Luc Heinrich
#
# This file is part of ruby-ffi.
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the Ruby FFI project nor the names of its contributors
#   may be used to endorse or promote products derived from this software
#   without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

module FFI

  # An instance of this class permits to manage {Enum}s. In fact, Enums is a collection of {Enum}s.
  class Enums

    # @return [nil]
    def initialize
      @all_enums = Array.new
      @tagged_enums = Hash.new
      @symbol_map = Hash.new
    end

    # @param [Enum] enum
    # Add an {Enum} to the collection.
    def <<(enum)
      @all_enums << enum
      @tagged_enums[enum.tag] = enum unless enum.tag.nil?
      @symbol_map.merge!(enum.symbol_map)
    end

    # @param query enum tag or part of an enum name
    # @return [Enum]
    # Find a {Enum} in collection.
    def find(query)
      if @tagged_enums.has_key?(query)
        @tagged_enums[query]
      else
        @all_enums.detect { |enum| enum.symbols.include?(query) }
      end
    end

    # @param symbol a symbol to find in merge symbol maps of all enums.
    # @return a symbol
    def __map_symbol(symbol)
      @symbol_map[symbol]
    end

  end

  # Represents a C enum.
  #
  # For a C enum:
  #  enum fruits {
  #    apple,
  #    banana,
  #    orange,
  #    pineapple
  #  };
  # are defined this vocabulary:
  # * a _symbol_ is a word from the enumeration (ie. _apple_, by example);
  # * a _value_ is the value of a symbol in the enumeration (by example, apple has value _0_ and banana _1_).
  class Enum
    include DataConverter

    attr_reader :tag

    # @param [nil, Enumerable] info
    # @param tag enum tag
    def initialize(info, tag=nil)
      @tag = tag
      @kv_map = Hash.new
      unless info.nil?
        last_cst = nil
        value = 0
        info.each do |i|
          case i
          when Symbol
            raise ArgumentError, "duplicate enum key" if @kv_map.has_key?(i)
            @kv_map[i] = value
            last_cst = i
            value += 1
          when Integer
            @kv_map[last_cst] = i
            value = i+1
          end
        end
      end
      @vk_map = @kv_map.invert
    end

    # @return [Array] enum symbol names
    def symbols
      @kv_map.keys
    end

    # Get a symbol or a value from the enum.
    # @overload [](query)
    #  Get enum value from symbol.
    #  @param [Symbol] query
    #  @return [Integer]
    # @overload [](query)
    #  Get enum symbol from value.
    #  @param [Integer] query
    #  @return [Symbol]
    def [](query)
      case query
      when Symbol
        @kv_map[query]
      when Integer
        @vk_map[query]
      end
    end
    alias find []

    # Get the symbol map.
    # @return [Hash]
    def symbol_map
      @kv_map
    end
    
    alias to_h symbol_map
    alias to_hash symbol_map

    # Get native type of Enum
    # @return [Type::INT]
    def native_type
      Type::INT
    end

    # @param [Symbol, Integer, #to_int] val
    # @param ctx unused
    # @return [Integer] value of a enum symbol
    def to_native(val, ctx)
      @kv_map[val] || if val.is_a?(Integer)
        val
      elsif val.respond_to?(:to_int)
        val.to_int
      else
        raise ArgumentError, "invalid enum value, #{val.inspect}"
      end
    end

    # @param val
    # @return symbol name if it exists for +val+.
    def from_native(val, ctx)
      @vk_map[val] || val
    end

  end

end
