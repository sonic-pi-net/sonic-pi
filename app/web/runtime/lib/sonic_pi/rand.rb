# SPDX-License-Identifier: AGPL-3.0-or-later
# Sonic Pi's random numbers, ported.
#
# There is no generator. Sonic Pi ships five tables of 441,000 numbers, one
# per noise colour, as 16-bit mono wav files under etc/buffers; a value is
# the sample divided by 32768. A thread's stream is a seed and an index into
# the table: the n-th draw reads table[(seed + n) mod 441000] and the index
# moves on. The seed can be a Float (a child thread's is a draw from its
# parent's stream, unrounded) and then the lookup truncates.
#
# Written for mruby: core classes only, and no Thread. The current stream is
# whatever the scheduler says it is (SonicPi::Rand.current).
module SonicPi
  module Rand
    SIZE = 441000
    SOURCES = [:white, :pink, :light_pink, :dark_pink, :perlin]
    FILES = { white: "rand-stream.wav", pink: "rand-stream-pink.wav", light_pink: "rand-stream-light-pink.wav",
              dark_pink: "rand-stream-dark-pink.wav", perlin: "rand-stream-perlin.wav" }

    # The five tables. The host hands the bytes over (install); natively they
    # can also be read from Sonic Pi's etc/buffers (load_dir).
    class Tables
      def initialize
        @tables = {}
      end

      def install(source, bytes)
        @tables[source.to_sym] = Table.new(bytes)
      end

      def [](source)
        @tables[source] or raise "random table #{source} is not installed"
      end

      def load_dir(dir)
        FILES.each { |source, file| install(source, data_chunk(File.open(File.join(dir, file), "rb") { |f| f.read })) }
        self
      end

      # The data chunk of a 16-bit PCM wav, kept as the bytes themselves: a
      # table is 882 KB either way, and an Array of 441,000 Floats is more
      # than a small mruby will allocate.
      def data_chunk(bytes)
        pos = 12
        while pos < bytes.bytesize
          id = bytes.byteslice(pos, 4)
          size = bytes.byteslice(pos + 4, 4).unpack("V")[0]
          return bytes.byteslice(pos + 8, [size, SIZE * 2].min) if id == "data"
          pos += 8 + size + (size & 1)
        end
        raise "no data chunk in the wav"
      end
    end

    class Table
      def initialize(bytes) = (@bytes = bytes)
      def size = @bytes.bytesize / 2
      # The i-th sample as a float in [-1, 1). The index wraps on the table's own length: a stream indexes
      # 0...SIZE whatever the table holds, and a table whose numbers repeat need only be shipped once over
      # (:white is one second of values tiled ten times, so the browser is sent 44,100 of them, not 441,000).
      def [](i)
        i %= size
        v = @bytes.getbyte(2 * i) | (@bytes.getbyte(2 * i + 1) << 8)
        v -= 65536 if v > 32767
        v / 32768.0
      end
    end

    # One thread's stream: where it is in which table.
    class State
      attr_accessor :seed, :idx, :source, :new_thread_idx

      def initialize(tables, seed = 0, idx = 0, source = :white, new_thread_idx = 0)
        @tables = tables
        @seed = seed
        @idx = idx
        @source = source
        @new_thread_idx = new_thread_idx   # how many children this thread has seeded
      end

      # no source (a run's own stream, or after clear) draws as :white
      def numbers = @tables[@source || :white]

      def rand!(max = 1, idx = nil)
        idx = inc_idx! unless idx
        rand_peek(max, idx)
      end

      def rand_peek(max = 1, idx = nil, seed = nil)
        idx = @idx unless idx
        seed = @seed unless seed
        i = seed + idx
        i = (i + 1) % SIZE
        numbers[i.to_i] * max
      end

      def rand_i!(max, idx = nil) = rand!(max, idx).to_i

      def inc_idx!(by = 1)
        r = @idx
        @idx += by
        r
      end

      def dec_idx!(by = 1)
        r = @idx
        @idx -= by
        r
      end

      def set_seed!(seed, idx = 0)
        @seed = seed
        @idx = idx
      end

      # The stream a thread spawned now would start with: a draw from this
      # thread's stream at its child counter, plus this thread's seed, in a
      # fresh index. seed: overrides the draw but not the addition. The child
      # counts its own children on from this thread's count, as Sonic Pi's
      # in_thread copies its locals after counting the new thread.
      def child(seed = nil)
        unless seed
          seed = rand!(SIZE, @new_thread_idx)
          @new_thread_idx += 1
        end
        State.new(@tables, seed + @seed, 0, @source, @new_thread_idx)
      end

      # Sonic Pi's Array#shuffle: reseed from the stream, swap pairs, then
      # return to the old stream one step on.
      def shuffle(array)
        orig_seed, orig_idx = @seed, @idx
        set_seed!(rand_i!(SIZE))
        a = array.dup
        s = a.size
        s.times do
          ia = rand!(s).to_i
          ib = rand!(s).to_i
          a[ia], a[ib] = a[ib], a[ia]
        end
        set_seed!(orig_seed, orig_idx + 1)
        a
      end
    end

    class << self
      attr_accessor :current
      def tables = (@tables ||= Tables.new)
    end
  end
end
