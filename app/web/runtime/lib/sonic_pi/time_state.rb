# SPDX-License-Identifier: AGPL-3.0-or-later
# What Time State holds: a value as bytes. set, cue and every cue from MIDI or OSC encode what they are given, and
# get decodes it, once, into frozen values, so what one thread stored no thread can change: a list comes back
# frozen, a Hash as a map, a String frozen; a node (a synth's, an FX's) is kept itself, as native keeps it. What cannot
# be encoded is what native cannot make thread safe (a lambda, an object): the same NotThreadSafeError.
#
# Native keeps the value itself, made thread safe (core.rb's __sp_make_thread_safe and sp_thread_safe?); safe?
# and the error messages here are those, and the bytes are how this runtime keeps it immutable.
module SonicPi
  module TimeState
    module_function

    # native's sp_thread_safe?: a value is already immutable (numbers, symbols, booleans, nil, frozen strings, and
    # frozen lists, rings and maps of those). cue's map form asks this of what it is given.
    def safe?(v)
      case v
      when Numeric, Symbol, TrueClass, FalseClass, NilClass then true
      when SonicPi::SynthNode, SonicPi::BlankNode then true   # native's Node#sp_thread_safe?: a node is safe to share
      when String then v.frozen?
      when SonicPi::Ring then v.to_a.all? { |x| safe?(x) }
      when SonicPi::SPMap then v.to_h.all? { |k, x| safe?(k) && safe?(x) }
      when Array then v.frozen? && v.all? { |x| safe?(x) }
      when Hash then v.frozen? && v.all? { |k, x| safe?(k) && safe?(x) }
      else false
      end
    end

    # native's __sp_make_thread_safe: the value as Time State keeps it (what get gives back)
    def make_safe(v) = decode(encode(v))

    def encode(v, out = String.new)
      case v
      when NilClass then out << "n"
      when TrueClass then out << "T"
      when FalseClass then out << "F"
      when Integer then out << "i" << v.to_s << "\0"          # any size: as text
      when Float then out << "d" << [v].pack("G")             # every bit of it
      when Rational then out << "q" << v.numerator.to_s << "/" << v.denominator.to_s << "\0"
      when Symbol then bytes(out, "y", v.to_s)
      when String then bytes(out, "s", v)
      when SonicPi::Ramp then items(out, "R", v.to_a)
      when SonicPi::Ring then items(out, "r", v.to_a)
      when SonicPi::SPMap then pairs(out, v.to_h)
      when Array then items(out, "a", v)
      when Hash then pairs(out, v)
      when SonicPi::SynthNode, SonicPi::BlankNode then out << "o" << keep(v).to_s << "\0"   # the node itself, as native keeps it
      else raise SonicPi::Core::NotThreadSafeError, "Sorry, unable to make a #{v.class} thread safe"
      end
      out
    end

    def decode(s) = read(s, 0)[0]

    # A node has no bytes of its own: Time State keeps the node, as native does (set :fx, fx; get(:fx) is that node),
    # by a number in its bytes. The newest KEPT are held; one stored and not read since is let go past that, so a loop
    # that stores a node each time round does not hold them all.
    KEPT = 4096
    @kept = {}      # number → node, oldest first
    @numbers = {}   # the node's object_id → its number: the same node stored again is the same number
    @kept_next = 0
    def keep(v)
      k = @numbers[v.object_id]
      return k if k && @kept[k].equal?(v)
      k = (@kept_next += 1)
      @kept[k] = v
      @numbers[v.object_id] = k
      while @kept.size > KEPT
        old, node = @kept.first
        @kept.delete(old)
        @numbers.delete(node.object_id) if @numbers[node.object_id] == old
      end
      k
    end
    def kept(k) = @kept[k]

    def bytes(out, tag, str) = (out << tag << [str.bytesize].pack("N") << (str.respond_to?(:b) ? str.b : str))   # its bytes, whatever its encoding
    def items(out, tag, list) = (out << tag << [list.size].pack("N"); list.each { |x| encode(x, out) })
    def pairs(out, h) = (out << "m" << [h.size].pack("N"); h.each { |k, x| encode(k, out); encode(x, out) })

    # the value at pos (a byte offset: the bytes are not text), and where the next begins
    def text_to_nul(s, pos)
      e = pos
      e += 1 until s.getbyte(e) == 0
      [s.byteslice(pos, e - pos), e + 1]
    end

    def read(s, pos)
      tag = s.byteslice(pos, 1)
      pos += 1
      case tag
      when "n" then [nil, pos]
      when "T" then [true, pos]
      when "F" then [false, pos]
      when "i" then t, pos = text_to_nul(s, pos); [t.to_i, pos]
      when "d" then [s.byteslice(pos, 8).unpack1("G"), pos + 8]
      when "q" then t, pos = text_to_nul(s, pos); n, d = t.split("/"); [Rational(n.to_i, d.to_i), pos]
      when "y", "s"
        n = s.byteslice(pos, 4).unpack1("N")
        str = s.byteslice(pos + 4, n)
        str.force_encoding("UTF-8") if str.respond_to?(:force_encoding)
        [tag == "y" ? str.to_sym : str.freeze, pos + 4 + n]
      when "a", "r", "R"
        n = s.byteslice(pos, 4).unpack1("N")
        pos += 4
        list = []
        n.times { x, pos = read(s, pos); list << x }
        list.freeze
        [tag == "a" ? list : tag == "r" ? SonicPi::Ring.new(list) : SonicPi::Ramp.new(list), pos]
      when "m"
        n = s.byteslice(pos, 4).unpack1("N")
        pos += 4
        h = {}
        n.times { k, pos = read(s, pos); x, pos = read(s, pos); h[k] = x }
        [SonicPi::SPMap.new(h), pos]
      when "o" then t, pos = text_to_nul(s, pos); [kept(t.to_i), pos]   # a node (keep)
      else raise "Time State: unreadable value at #{pos - 1}"
      end
    end
  end
end
