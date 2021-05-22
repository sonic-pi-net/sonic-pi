#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2020 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++


module SonicPi
  module OSC
    class Blob

      attr_reader :binary
      def initialize(data)
        b = ([data.size].pack('N') + data).force_encoding("BINARY")
        # add padding

        @binary = b + ("\000" * ((4 - (b.size % 4)) % 4))

      end

      def to_s
        @binary.to_s
      end

      def inspect
        @binary.inspect
      end

    end

    class Int64
      attr_reader :binary
      def initialize(val)
        @val = val
        @binary = [val].pack('q>')
      end

      def to_i
        @val.to_i
      end

      def inspect
        @val.inspect
      end
    end
  end
end
