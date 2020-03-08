require_relative "../note"
require_relative "../scale"
require_relative "../chord"
require_relative "../chordgroup"
require_relative "support/docsystem"

class SonicPi::Core::SPVector
  def notes(*args)

    note = lambda do |n, args|
      # code copied from the notes fn in this namespace
      case n
      when Numeric
        return n
      when Symbol
        return nil if(n == :r || n == :rest)
      when NilClass
        return nil
      when Proc
        return note(n.call, *args)
      when Hash
        raise "Unable to create a note from the Map: #{n.inspect}"
      end

      return SonicPi::Note.resolve_midi_note_without_octave(n) if args.empty?

      args_h = resolve_synth_opts_hash_or_array(args)
      octave = args_h[:octave]
      if octave
        SonicPi::Note.resolve_midi_note(n, octave)
      else
        SonicPi::Note.resolve_midi_note_without_octave(n)
      end
    end

    self.map {|n| note.call(n, args) }
  end
end

class Symbol
  def -(other)
    return self if (self == :r) || (self == :rest)
    SonicPi::Note.resolve_midi_note_without_octave(self) - SonicPi::Note.resolve_midi_note_without_octave(other)
  end

  def +(other)
    return self if (self == :r) || (self == :rest)
    SonicPi::Note.resolve_midi_note_without_octave(self) + SonicPi::Note.resolve_midi_note_without_octave(other)
  end

  def to_f
    return 0.0 if (self == :r) || (self == :rest)
    SonicPi::Note.resolve_midi_note_without_octave(self).to_f
  end

  def to_i
    self.to_f.to_i
  end
end

class NilClass
  def -(other)
    return nil
  end

  def +(other)
    return nil
  end
end

module SonicPi
  module Lang
    module WesternTheory


      include SonicPi::Lang::Support::DocSystem

      def octs(start, num_octs=1)
        a = []
        num_octs.times do |i|
          a << (note(start) + (12 * i))
        end
        a.ring
      end
      doc name:           :octs,
      introduced:     Version.new(2,8,0),
      summary:        "Create a ring of octaves",
      args:           [[:start, :note], [:num_octaves, :pos_int]],
      returns:        :ring,
      opts:           nil,
      accepts_block:  false,
      doc:            "Create a ring of successive octaves starting at `start` for `num_octaves`. ",
      examples:       [
        "(octs 60, 2)  #=> (ring 60, 72)",
        "(octs :e3, 3) #=> (ring 52, 64, 76)"
      ]

      def midi_notes(*args)
        args = args.map {|a| note(a)}
        SonicPi::Core::RingVector.new(args)
      end
      doc name:           :midi_notes,
      introduced:     Version.new(2,7,0),
      summary:        "Create a ring buffer of midi note numbers",
      args:           [[:list, :array]],
      returns:        :ring,
      opts:           nil,
      accepts_block:  false,
      memoize: true,
      doc:            "Create a new immutable ring buffer of notes from args. Indexes wrap around positively and negatively. Final ring consists only of MIDI numbers and nil.",
      examples:       [
        "(midi_notes :d3, :d4, :d5) #=> (ring 50, 62, 74)",
        "(midi_notes :d3, 62,  nil) #=> (ring 50, 62, nil)"
      ]


      def rest?(n)
        case n
        when Numeric
          return false
        when Symbol
          return n == :r || n == :rest
        when NilClass
          return true
        when Hash
          if n.has_key?(:note)
            note = n[:note]
            return (note.nil? || note == :r || note == :rest)
          else
            return false
          end
        else
          return false
        end
      end
      doc name:          :rest?,
      introduced:    Version.new(2,1,0),
      summary:       "Determine if note or args is a rest",
      doc:           "Given a note or an args map, returns true if it represents a rest and false if otherwise",
      args:          [[:note_or_args, :number_symbol_or_map]],
      accepts_block: false,
      examples:      ["puts rest? nil # true",
        "puts rest? :r # true",
        "puts rest? :rest # true",
        "puts rest? 60 # false",
        "puts rest? {} # false",
        "puts rest? {note: :rest} # true",
        "puts rest? {note: nil} # true",
        "puts rest? {note: 50} # false"]


      def pitch_ratio(*args)
        raise "The fn pitch_ratio has been renamed. Please use the new name: pitch_to_ratio"
      end

      def pitch_to_ratio(m)
        2.0 ** (m.to_f / 12.0)
      end
      doc name:          :pitch_to_ratio,
      introduced:    Version.new(2,5,0),
      summary:       "relative MIDI pitch to frequency ratio",
      doc:           "Convert a midi note to a ratio which when applied to a frequency will scale the frequency by the number of semitones. Useful for changing the pitch of a sample by using it as a way of generating the rate.",
      args:          [[:pitch, :midi_number]],
      opts:          nil,
      accepts_block: false,
      examples:      [
        "pitch_to_ratio 12 #=> 2.0",
        "pitch_to_ratio 1 #=> 1.05946",
        "pitch_to_ratio -12 #=> 0.5",
        "sample :ambi_choir, rate: pitch_to_ratio(3) # Plays :ambi_choir 3 semitones above default.",
        "
# Play a chromatic scale of semitones
(range 0, 16).each do |n|                  # For each note in the range 0->16
  sample :ambi_choir, rate: pitch_to_ratio(n) # play :ambi_choir at the relative pitch
  sleep 0.5                                # and wait between notes
end"
      ]




      def ratio_to_pitch(r)
        12.0 * Math.log2(r.abs.to_f)
      end
      doc name:          :ratio_to_pitch,
      introduced:    Version.new(2,7,0),
      summary:       "relative frequency ratio to MIDI pitch",
      doc:           "Convert a frequency ratio to a midi note which when added to a note will transpose the note to match the frequency ratio.",
      args:          [[:ratio, :number]],
      opts:          nil,
      accepts_block: false,
      examples:      [
        "ratio_to_pitch 2 #=> 12.0",
        "ratio_to_pitch 0.5 #=> -12.0"

      ]




      def midi_to_hz(n)
        n = note(n) unless n.is_a? Numeric
        440.0 * (2 ** ((n - 69) / 12.0))
      end
      doc name:          :midi_to_hz,
      introduced:    Version.new(2,0,0),
      summary:       "MIDI to Hz conversion",
      doc:           "Convert a midi note to hz",
      args:          [[:note, :symbol_or_number]],
      opts:          nil,
      accepts_block: false,
      examples:      ["midi_to_hz(60) #=> 261.6256"]




      def hz_to_midi(freq)
        (12 * (Math.log(freq * 0.0022727272727) / Math.log(2))) + 69
      end
      doc name:          :hz_to_midi,
      introduced:    Version.new(2,0,0),
      summary:       "Hz to MIDI conversion",
      doc:           "Convert a frequency in hz to a midi note. Note that the result isn't an integer and there is a potential for some very minor rounding errors.",
      args:          [[:freq, :number]],
      opts:          nil,
      accepts_block: false,
      examples:      ["hz_to_midi(261.63) #=> 60.0003"]




      def set_cent_tuning!(shift)
        @mod_sound_studio.cent_tuning = shift
      end
      doc name:          :set_cent_tuning!,
      introduced:    Version.new(2,10,0),
      summary:       "Global Cent tuning",
      doc:           "Globally tune Sonic Pi to play with another external instrument.

Uniformly tunes your music by shifting all notes played by the specified number of cents. To shift up by a cent use a cent tuning of 1. To shift down use negative numbers. One semitone consists of 100 cents.

See `use_cent_tuning` for setting the cent tuning value locally for a specific thread or `live_loop`. This is a global value and will shift the tuning for *all* notes. It will also persist for the entire session.

Important note: the cent tuning set by `set_cent_tuning!` is independent of any thread-local cent tuning values set by `use_cent_tuning` or `with_cent_tuning`. ",
      args:          [[:cent_shift, :number]],
      opts:          nil,
      accepts_block: false,
      intro_fn:       false,
      examples:      ["
play 50 # Plays note 50
set_cent_tuning! 1
play 50 # Plays note 50.01"]

      def use_cent_tuning(shift, &block)
        raise "use_cent_tuning does not work with a do/end block. Perhaps you meant with_cent_tuning" if block
        raise "Cent tuning value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        __thread_locals.set(:sonic_pi_mod_sound_cent_tuning, shift)
      end
      doc name:          :use_cent_tuning,
      introduced:    Version.new(2,9,0),
      summary:       "Cent tuning",
      doc:           "Uniformly tunes your music by shifting all notes played by the specified number of cents. To shift up by a cent use a cent tuning of 1. To shift down use negative numbers. One semitone consists of 100 cents.

See `with_cent_tuning` for setting the cent tuning value only for a specific `do`/`end` block. To transpose entire semitones see `use_transpose`.",
      args:          [[:cent_shift, :number]],
      opts:          nil,
      accepts_block: false,
      intro_fn:       true,
      examples:      ["
play 50 # Plays note 50
use_cent_tuning 1
play 50 # Plays note 50.01"]




      def with_cent_tuning(shift, &block)
        raise "with_cent_tuning requires a do/end block. Perhaps you meant use_cent_tuning" unless block
        raise "Cent tuning value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        curr = __thread_locals.get(:sonic_pi_mod_sound_cent_tuning)
        __thread_locals.set(:sonic_pi_mod_sound_cent_tuning, shift)
        res = block.call
        __thread_locals.set(:sonic_pi_mod_sound_cent_tuning, curr)
        res
      end
      doc name:           :with_cent_tuning,
      introduced:     Version.new(2,9,0),
      summary:        "Block-level cent tuning",
      doc:            "Similar to `use_cent_tuning` except only applies cent shift to code within supplied `do`/`end` block. Previous cent tuning value is restored after block. One semitone consists of 100 cents. To transpose entire semitones see `with_transpose`.",
      args:           [[:cent_shift, :number]],
      opts:           nil,
      accepts_block:  true,
      requires_block: true,
      examples:       ["
use_cent_tuning 1
play 50 # Plays note 50.01

with_cent_tuning 2 do
  play 50 # Plays note 50.02
end

# Original cent tuning value is restored
play 50 # Plays note 50.01

"]



      def use_octave(shift, &block)
        raise "use_octave does not work with a do/end block. Perhaps you meant with_octave" if block
        raise "Octave shift must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        __thread_locals.set(:sonic_pi_mod_sound_octave_shift, shift)
      end
      doc name:          :use_octave,
      introduced:    Version.new(2,9,0),
      summary:       "Note octave transposition",
      doc:           "Transposes your music by shifting all notes played by the specified number of octaves. To shift up by an octave use a transpose of 1. To shift down use negative numbers. See `with_octave` for setting the octave shift only for a specific `do`/`end` block. For transposing the notes within the octave range see `use_transpose`.",
      args:          [[:octave_shift, :number]],
      opts:          nil,
      accepts_block: false,
      intro_fn:      true,
      examples:      ["
play 50 # Plays note 50
use_octave 1
play 50 # Plays note 62",

        "
# You may change the transposition multiple times:
play 62 # Plays note 62
use_octave -1
play 62 # Plays note 50
use_octave 2
play 62 # Plays note 86"]



      def with_octave(shift, &block)
        raise "with_octave requires a do/end block. Perhaps you meant use_octave" unless block
        raise "Octave shift must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        curr = __thread_locals.get(:sonic_pi_mod_sound_octave_shift)
        __thread_locals.set(:sonic_pi_mod_sound_octave_shift, shift)
        res = block.call
        __thread_locals.set(:sonic_pi_mod_sound_octave_shift, curr)
        res
      end
      doc name:          :with_octave,
      introduced:    Version.new(2,9,0),
      summary:       "Block level octave transposition",
      doc:           "Transposes your music by shifting all notes played by the specified number of octaves within the specified block. To shift up by an octave use a transpose of 1. To shift down use negative numbers. For transposing the notes within the octave range see `with_transpose`.",
      args:          [[:octave_shift, :number]],
      opts:          nil,
      accepts_block: true,
      intro_fn:      true,
      examples:      ["
play 50 # Plays note 50
sleep 1
with_octave 1 do
 play 50 # Plays note 62
end
sleep 1
play 50 # Plays note 50"]




      def use_transpose(shift, &block)
        raise "use_transpose does not work with a do/end block. Perhaps you meant with_transpose" if block
        raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        __thread_locals.set(:sonic_pi_mod_sound_transpose, shift)
      end
      doc name:          :use_transpose,
      introduced:    Version.new(2,0,0),
      summary:       "Note transposition",
      doc:           "Transposes your music by shifting all notes played by the specified amount. To shift up by a semitone use a transpose of 1. To shift down use negative numbers. See `with_transpose` for setting the transpose value only for a specific `do`/`end` block. To transpose entire octaves see `use_octave`.",
      args:          [[:note_shift, :number]],
      opts:          nil,
      accepts_block: false,
      intro_fn:       true,
      examples:      ["
play 50 # Plays note 50
use_transpose 1
play 50 # Plays note 51",

        "
# You may change the transposition multiple times:
play 62 # Plays note 62
use_transpose -12
play 62 # Plays note 50
use_transpose 3
play 62 # Plays note 65"]




      def with_transpose(shift, &block)
        raise "with_transpose requires a do/end block. Perhaps you meant use_transpose" unless block
        raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        curr = __thread_locals.get(:sonic_pi_mod_sound_transpose)
        __thread_locals.set(:sonic_pi_mod_sound_transpose, shift)
        res = block.call
        __thread_locals.set(:sonic_pi_mod_sound_transpose, curr)
        res
      end
      doc name:           :with_transpose,
      introduced:     Version.new(2,0,0),
      summary:        "Block-level note transposition",
      doc:            "Similar to use_transpose except only applies to code within supplied `do`/`end` block. Previous transpose value is restored after block. To transpose entire octaves see `with_octave`.",
      args:           [[:note_shift, :number]],
      opts:           nil,
      accepts_block:  true,
      requires_block: true,
      examples:       ["
use_transpose 3
play 62 # Plays note 65

with_transpose 12 do
  play 50 # Plays note 62
  sleep 1
  play 72 # Plays note 84
end

# Original transpose value is restored
play 80 # Plays note 83

"]

      def use_tuning(tuning, fundamental_note = :c, &block)
        raise "use_tuning does not work with a do/end block. Perhaps you meant with_tuning" if block
        raise "tuning value must be a symbol like :just or :equal, got #{tuning.inspect}" unless tuning.is_a?(Symbol)
        __thread_locals.set(:sonic_pi_mod_sound_tuning, [tuning, fundamental_note].freeze)
      end
      doc name:          :use_tuning,
      introduced:    Version.new(2,6,0),
      summary:       "Use alternative tuning systems",
      doc:           "In most music we make semitones by dividing the octave into 12 equal parts, which is known as equal temperament. However there are lots of other ways to tune the 12 notes. This method adjusts each midi note into the specified tuning system. Because the ratios between notes aren't always equal, be careful to pick a centre note that is in the key of the music you're making for the best sound. Currently available tunings are `:just`, `:pythagorean`, `:meantone` and the default of `:equal`",
      args:          [[:tuning, :symbol], [:fundamental_note, :symbol_or_number]],
      opts:          nil,
      accepts_block: false,
      examples:      ["
play :e4 # Plays note 64
use_tuning :just, :c
play :e4 # Plays note 63.8631
# transparently changes midi notes too
play 64 # Plays note 63.8631",

        "
# You may change the tuning multiple times:
play 64 # Plays note 64
use_tuning :just
play 64 # Plays note 63.8631
use_tuning :equal
play 64 # Plays note 64"]



      def with_tuning(tuning, fundamental_note = :c, &block)
        raise "with_tuning requires a do/end block. Perhaps you meant use_tuning" unless block
        raise "tuning value must be a symbol like :just or :equal, got #{tuning.inspect}" unless tuning.is_a?(Symbol)
        curr_tuning_info = __thread_locals.get(:sonic_pi_mod_sound_tuning)
        __thread_locals.set(:sonic_pi_mod_sound_tuning, [tuning, fundamental_note].freeze)
        res = block.call
        __thread_locals.set(:sonic_pi_mod_sound_tuning, curr_tuning_info)
        res
      end
      doc name:          :with_tuning,
      introduced:    Version.new(2,6,0),
      summary:       "Block-level tuning modification",
      doc:           "Similar to use_tuning except only applies to code within supplied `do`/`end` block. Previous tuning value is restored after block.",
      args:          [[:tuning, :symbol], [:fundamental_note, :symbol_or_number]],
      opts:          nil,
      accepts_block: true,
      examples:      ["
use_tuning :equal, :c
play :e4 # Plays note 64
with_tuning :just, :c do
  play :e4 # Plays note 63.8631
  sleep 1
  play :c4 # Plays note 60
end
# Original tuning value is restored
play :e4 # Plays note 64"]


      def current_transpose
        __thread_locals.get(:sonic_pi_mod_sound_transpose) || 0
      end
      doc name:          :current_transpose,
      introduced:    Version.new(2,0,0),
      summary:       "Get current transposition",
      doc:           "Returns the current transpose value.

This can be set via the fns `use_transpose` and `with_transpose`.",
      args:          [],
      opts:          nil,
      accepts_block: false,
      examples:      ["
puts current_transpose # Print out the current transpose value"]




      def current_cent_tuning
        __thread_locals.get(:sonic_pi_mod_sound_cent_tuning) || 0
      end
      doc name:          :current_cent_tuning,
      introduced:    Version.new(2,9,0),
      summary:       "Get current cent shift",
      doc:           "Returns the cent shift value.

This can be set via the fns `use_cent_tuning` and `with_cent_tuning`.",
      args:          [],
      opts:          nil,
      accepts_block: false,
      examples:      ["
puts current_cent_tuning # Print out the current cent shift"]




      def current_octave
        __thread_locals.get(:sonic_pi_mod_sound_octave_shift) || 0
      end
      doc name:          :current_octave,
      introduced:    Version.new(2,9,0),
      summary:       "Get current octave shift",
      doc:           "Returns the octave shift value.

This can be set via the fns `use_octave` and `with_octave`.",
      args:          [],
      opts:          nil,
      accepts_block: false,
      examples:      ["
puts current_octave # Print out the current octave shift"]




      def note(n, *args)
        # Short circuit out if possible.
        # Also recurse if necessary.
        case n
        when Numeric
          return n
        when Symbol
          return nil if(n == :r || n == :rest)
        when NilClass
          return nil
        when Proc
          return note(n.call, *args)
        when Hash
          raise "Unable to create a note from the Map: #{n.inspect}"
        end

        return Note.resolve_midi_note_without_octave(n) if args.empty?

        args_h = resolve_synth_opts_hash_or_array(args)
        octave = args_h[:octave]
        if octave
          Note.resolve_midi_note(n, octave)
        else
          Note.resolve_midi_note_without_octave(n)
        end
      end
      doc name:          :note,
      introduced:    Version.new(2,0,0),
      summary:       "Describe note",
      doc:           "Takes a midi note, a symbol (e.g. `:C`) or a string (e.g. `\"C\"`) and resolves it to a midi note. You can also pass an optional `octave:` parameter to get the midi note for a given octave. Please note - `octave:` param overrides any octave specified in a symbol i.e. `:c3`. If the note is `nil`, `:r` or `:rest`, then `nil` is returned (`nil` represents a rest)",
      args:          [[:note, :symbol_or_number]],
      opts:          {:octave => "The octave of the note. Overrides any octave declaration in the note symbol such as :c2. Default is 4"},
      accepts_block: false,
      examples:      ["
# These all return 60 which is the midi number for middle C (octave 4)
puts note(60)
puts note(:C)
puts note(:C4)
puts note('C')
",
        "# returns 60 - octave param has no effect if we pass in a number
puts note(60, octave: 2)

# These all return 36 which is the midi number for C2 (two octaves below middle C)
puts note(:C, octave: 2)
puts note(:C4, octave: 2) # note the octave param overrides any octaves specified in a symbol
puts note('C', octave: 2)
"]




      def note_range(low_note, high_note, *opts)
        opts_h = resolve_synth_opts_hash_or_array(opts)
        low_note = note(low_note)
        high_note = note(high_note)

        potential_note_range = Range.new(low_note, high_note)

        if opts_h[:pitches]
          pitch_classes = opts_h[:pitches].map {|x| Note.resolve_note_name(x) }

          note_pool = potential_note_range.select {|n|
            pitch_classes.include? Note.resolve_note_name(n)
          }
        else
          note_pool = potential_note_range
        end

        note_pool.ring
      end
      doc name:           :note_range,
      introduced:     Version.new(2,6,0),
      summary:        "Get a range of notes",
      args:           [[:low_note, :note], [:high_note, :note]],
      returns:        :ring,
      opts:           {:pitches => "An array of notes (symbols or ints) to filter on. Octave information is ignored."},
      accepts_block:  false,
      doc:            "Produces a ring of all the notes between a low note and a high note. By default this is chromatic (all the notes) but can be filtered with a pitches: argument. This opens the door to arpeggiator style sequences and other useful patterns. If you try to specify only pitches which aren't in the range it will raise an error - you have been warned!",
      examples:       [
        "(note_range :c4, :c5) # => (ring 60,61,62,63,64,65,66,67,68,69,70,71,72)",
        "(note_range :c4, :c5, pitches: (chord :c, :major)) # => (ring 60,64,67,72)",
        "(note_range :c4, :c6, pitches: (chord :c, :major)) # => (ring 60,64,67,72,76,79,84)",
        "(note_range :c4, :c5, pitches: (scale :c, :major)) # => (ring 60,62,64,65,67,69,71,72)",
        "(note_range :c4, :c5, pitches: [:c4, :g2]) # => (ring 60,67,72)",
        "live_loop :arpeggiator do
  # try changing the chord
  play (note_range :c4, :c5, pitches: (chord :c, :major)).tick
  sleep 0.125
end"
      ]






      def note_info(n, args=nil)
        raise Exception.new("note_info argument must be a valid note. Got nil.") if(n.nil?)

        return SonicPi::Note.resolve_note(n) if(args.nil?)

        args_h = resolve_synth_opts_hash_or_array(args)
        SonicPi::Note.resolve_note(n, args_h[:octave])
      end
      doc name:          :note_info,
      introduced:    Version.new(2,0,0),
      summary:       "Get note info",
      doc:           "Returns an instance of `SonicPi::Note`. Please note - `octave:` param overrides any octave specified in a symbol i.e. `:c3`",
      args:          [[:note, :symbol_or_number]],
      opts:          {:octave => "The octave of the note. Overrides any octave declaration in the note symbol such as :c2. Default is 4"},
      accepts_block: false,
      examples:      [%Q{
puts note_info(:C, octave: 2)
# returns #<SonicPi::Note :C2>}]





      def degree(degree, tonic, scale)
        Scale.resolve_degree(degree, tonic, scale)
      end
      doc name:           :degree,
      introduced:         Version.new(2,1,0),
      summary:            "Convert a degree into a note",
      doc:                "For a given scale and tonic it takes a symbol/string/number and resolves it to a midi note. The degree can be either a decimal number or a roman numeral (if it's a string or symbol), and may optionally be prefixed an augmentation (`a`/`d` for an augmented/diminished interval, `aa`/`dd` for double augmented/diminished or `p` for a perfect (unchanged) interval).",
      args:               [[:degree, :symbol_or_number], [:tonic, :symbol], [:scale, :symbol]],
      accepts_block:      false,
      examples:           [%Q{
play degree(:iii, :D3, :major) # major third up from :D3
play degree(3, :C3, :minor) # minor third up from :C3
play degree('d5', :B3, :major) # diminished fifth up from :B3
},
        %q{
chrd = []
[:i, :iii, :v, :dvii, :dix, :Axi, :xiii].each do |d|  # for each degree in the chord
  chrd.append (degree d, :Fs, :major)  # add the corresponding note
end
play chrd  # play an F# 13+11-9 chord, using roman numeral symbols
},
        %Q{
chrd = []
['1', '3', '5', 'd7', 'd9', 'A11', '13'].each do |d|
  chrd.append (degree d, :Fs, :major)
end
play chrd  # the same chord as above, but using decimal number strings
}]




      def scale(tonic_or_name, name_or_opts=nil, opts=nil)
        if name_or_opts.nil? || name_or_opts.is_a?(Hash)
          tonic = 0
          name = tonic_or_name
          num_octaves = (name_or_opts.is_a?(Hash) && name_or_opts[:num_octaves]) || 1
        else
          tonic = tonic_or_name
          name = name_or_opts
          num_octaves = (opts.is_a?(Hash) && opts[:num_octaves]) || 1
        end

        Scale.resolve_scale(tonic, name, num_octaves).ring
      end
      doc name:          :scale,
      introduced:    Version.new(2,0,0),
      summary:       "Create scale",
      doc:           "Creates a ring of MIDI note numbers when given a tonic note and a scale name. Also takes an optional `num_octaves:` parameter (octave `1` is the default). If only passed the scale name, the tonic defaults to 0. See examples.",
      args:          [[:tonic, :symbol], [:name, :symbol]],
      returns:        :ring,
      opts:          {:num_octaves => "The number of octaves you'd like the scale to consist of. More octaves means a larger scale. Default is 1."},
      accepts_block: false,
      intro_fn:       true,
      memoize:        true,
      examples:      ["
puts (scale :C, :major) # returns the following ring of MIDI note numbers: (ring 60, 62, 64, 65, 67, 69, 71, 72)",
        "# anywhere you can use a list or ring of notes, you can also use scale
play_pattern (scale :C, :major)",
        "# you can use the :num_octaves parameter to get more notes
play_pattern (scale :C, :major, num_octaves: 2)",
        "# Scales can start with any note:
puts (scale 50, :minor) #=> (ring 50, 52, 53, 55, 57, 58, 60, 62)
puts (scale 50.1, :minor) #=> (ring 50.1, 52.1, 53.1, 55.1, 57.1, 58.1, 60.1, 62.1)
puts (scale :minor) #=> (ring 0, 2, 3, 5, 7, 8, 10, 12)",


        " # scales are also rings
live_loop :scale_player do
  play (scale :Eb3, :super_locrian).tick, release: 0.1
  sleep 0.125
end",

        " # scales starting with 0 are useful in combination with sample's rpitch:
live_loop :scaled_sample do
  sample :bass_trance_c, rpitch: (scale 0, :minor).tick
  sleep 1
end",


        "# Sonic Pi supports a large range of scales:

(scale :C, :diatonic)
(scale :C, :ionian)
(scale :C, :major)
(scale :C, :dorian)
(scale :C, :phrygian)
(scale :C, :lydian)
(scale :C, :mixolydian)
(scale :C, :aeolian)
(scale :C, :minor)
(scale :C, :locrian)
(scale :C, :hex_major6)
(scale :C, :hex_dorian)
(scale :C, :hex_phrygian)
(scale :C, :hex_major7)
(scale :C, :hex_sus)
(scale :C, :hex_aeolian)
(scale :C, :minor_pentatonic)
(scale :C, :yu)
(scale :C, :major_pentatonic)
(scale :C, :gong)
(scale :C, :egyptian)
(scale :C, :shang)
(scale :C, :jiao)
(scale :C, :zhi)
(scale :C, :ritusen)
(scale :C, :whole_tone)
(scale :C, :whole)
(scale :C, :chromatic)
(scale :C, :harmonic_minor)
(scale :C, :melodic_minor_asc)
(scale :C, :hungarian_minor)
(scale :C, :octatonic)
(scale :C, :messiaen1)
(scale :C, :messiaen2)
(scale :C, :messiaen3)
(scale :C, :messiaen4)
(scale :C, :messiaen5)
(scale :C, :messiaen6)
(scale :C, :messiaen7)
(scale :C, :super_locrian)
(scale :C, :hirajoshi)
(scale :C, :kumoi)
(scale :C, :neapolitan_major)
(scale :C, :bartok)
(scale :C, :bhairav)
(scale :C, :locrian_major)
(scale :C, :ahirbhairav)
(scale :C, :enigmatic)
(scale :C, :neapolitan_minor)
(scale :C, :pelog)
(scale :C, :augmented2)
(scale :C, :scriabin)
(scale :C, :harmonic_major)
(scale :C, :melodic_minor_desc)
(scale :C, :romanian_minor)
(scale :C, :hindu)
(scale :C, :iwato)
(scale :C, :melodic_minor)
(scale :C, :diminished2)
(scale :C, :marva)
(scale :C, :melodic_major)
(scale :C, :indian)
(scale :C, :spanish)
(scale :C, :prometheus)
(scale :C, :diminished)
(scale :C, :todi)
(scale :C, :leading_whole)
(scale :C, :augmented)
(scale :C, :purvi)
(scale :C, :chinese)
(scale :C, :lydian_minor)
(scale :C, :blues_major)
(scale :C, :blues_minor)
"]




      def chord_degree(degree, tonic, scale=:major, number_of_notes=4, *opts)
        opts = resolve_synth_opts_hash_or_array(opts)
        opts = {invert: 0}.merge(opts)

        chord_invert(Chord.resolve_degree(degree, tonic, scale, number_of_notes), opts[:invert]).ring
      end
      doc name:          :chord_degree,
      introduced:    Version.new(2,1,0),
      summary:       "Construct chords of stacked thirds, based on scale degrees",
      doc:           "In music we build chords from scales. For example, a C major chord is made by taking the 1st, 3rd and 5th notes of the C major scale (C, E and G). If you do this on a piano you might notice that you play one, skip one, play one, skip one etc. If we use the same spacing and start from the second note in C major (which is a D), we get a D minor chord which is the 2nd, 4th and 6th notes in C major (D, F and A). We can move this pattern all the way up or down the scale to get different types of chords. `chord_degree` is a helper method that returns a ring of midi note numbers when given a degree (starting point in a scale) which is a symbol `:i`, `:ii`, `:iii`, `:iv`, `:v`, `:vi`, `:vii` or a number `1`-`7`. The second argument is the tonic note of the scale, the third argument is the scale type and finally the fourth argument is number of notes to stack up in the chord. If we choose 4 notes from degree `:i` of the C major scale, we take the 1st, 3rd, 5th and 7th notes of the scale to get a C major 7 chord.",
      args:          [[:degree, :symbol_or_number], [:tonic, :symbol], [:scale, :symbol], [:number_of_notes, :number]],
      returns:       :ring,
      opts:          { invert: "Apply the specified num inversions to chord. See the fn `chord_invert`." },
      accepts_block: false,
      memoize:       true,
      examples:      ["puts (chord_degree :i, :A3, :major) # returns a ring of midi notes - (ring 57, 61, 64, 68) - an A major 7 chord",
        "play (chord_degree :i, :A3, :major, 3)",
        "play (chord_degree :ii, :A3, :major, 3) # Chord ii in A major is a B minor chord",
        "play (chord_degree :iii, :A3, :major, 3) # Chord iii in A major is a C# minor chord",
        "play (chord_degree :iv, :A3, :major, 3) # Chord iv in A major is a D major chord",
        "play (chord_degree :i, :C4, :major, 4) # Taking four notes is the default. This gives us 7th chords - here it plays a C major 7",
        "play (chord_degree :i, :C4, :major, 5) # Taking five notes gives us 9th chords - here it plays a C major 9 chord",
        "play (chord_degree :i, :C4, :major, 3, invert: 1) # Play the first inversion of chord i in C major - (ring 64, 67, 72)"
      ]




      def chord(tonic_or_name, *opts)
        tonic = 0
        name = :minor
        if opts.size == 0
          name = tonic_or_name
        elsif (opts.size == 1) && opts[0].is_a?(Hash)
          name = tonic_or_name
        else
          tonic = tonic_or_name
          name = opts.shift
        end

        return [] unless tonic
        opts = resolve_synth_opts_hash_or_array(opts)
        c = []
        if is_list_like?(tonic)
          raise "List passed as parameter to chord needs two elements i.e. (chord [:e3, :minor]), you passed: #{tonic.inspect}" unless tonic.size == 2
          c = Chord.new(tonic[0], tonic[1], opts[:num_octaves])
        else
          c = Chord.new(tonic, name, opts[:num_octaves])
        end
        c = chord_invert(c, opts[:invert]) if opts[:invert]
        return c.ring
      end
      doc name:          :chord,
      introduced:    Version.new(2,0,0),
      summary:       "Create chord",
      doc:           "Creates an immutable ring of Midi note numbers when given a tonic note and a chord type. If only passed a chord type, will default the tonic to 0. See examples.",
      args:          [[:tonic, :symbol], [:name, :symbol]],
      returns:        :ring,
      opts:          {invert: "Apply the specified num inversions to chord. See the fn `chord_invert`.",
        num_octaves:   "Create an arpeggio of the chord over n octaves"},
      accepts_block: false,
      intro_fn:      true,
      memoize:       true,
      examples:      ["
puts (chord :e, :minor) # returns a ring of midi notes - (ring 64, 67, 71)
",
        "# Play all the notes together
play (chord :e, :minor)",
        "
# Chord inversions (see the fn chord_invert)
play (chord :e3, :minor, invert: 0) # Play the basic :e3, :minor chord - (ring 52, 55, 59)
play (chord :e3, :minor, invert: 1) # Play the first inversion of :e3, :minor - (ring 55, 59, 64)
play (chord :e3, :minor, invert: 2) # Play the first inversion of :e3, :minor - (ring 59, 64, 67)
",

        "# You can create a chord without a tonic:
puts (chord :minor) #=> (ring 0, 3, 7)",

        "# chords are great for arpeggiators
live_loop :arp do
  play chord(:e, :minor, num_octaves: 2).tick, release: 0.1
  sleep 0.125
end",
        "# Sonic Pi supports a large range of chords
 # Notice that the more exotic ones have to be surrounded by ' quotes
(chord :C, '1')
(chord :C, '5')
(chord :C, '+5')
(chord :C, 'm+5')
(chord :C, :sus2)
(chord :C, :sus4)
(chord :C, '6')
(chord :C, :m6)
(chord :C, '7sus2')
(chord :C, '7sus4')
(chord :C, '7-5')
(chord :C, 'm7-5')
(chord :C, '7+5')
(chord :C, 'm7+5')
(chord :C, '9')
(chord :C, :m9)
(chord :C, 'm7+9')
(chord :C, :maj9)
(chord :C, '9sus4')
(chord :C, '6*9')
(chord :C, 'm6*9')
(chord :C, '7-9')
(chord :C, 'm7-9')
(chord :C, '7-10')
(chord :C, '9+5')
(chord :C, 'm9+5')
(chord :C, '7+5-9')
(chord :C, 'm7+5-9')
(chord :C, '11')
(chord :C, :m11)
(chord :C, :maj11)
(chord :C, '11+')
(chord :C, 'm11+')
(chord :C, '13')
(chord :C, :m13)
(chord :C, :add2)
(chord :C, :add4)
(chord :C, :add9)
(chord :C, :add11)
(chord :C, :add13)
(chord :C, :madd2)
(chord :C, :madd4)
(chord :C, :madd9)
(chord :C, :madd11)
(chord :C, :madd13)
(chord :C, :major)
(chord :C, :M)
(chord :C, :minor)
(chord :C, :m)
(chord :C, :major7)
(chord :C, :dom7)
(chord :C, '7')
(chord :C, :M7)
(chord :C, :minor7)
(chord :C, :m7)
(chord :C, :augmented)
(chord :C, :a)
(chord :C, :diminished)
(chord :C, :dim)
(chord :C, :i)
(chord :C, :diminished7)
(chord :C, :dim7)
(chord :C, :i7)
"]




      def chord_invert(notes, shift)
        raise "Inversion shift value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
        shift = shift.round
        raise "Notes must be a list of notes, got #{notes.inspect}" unless is_list_like?(notes)
        if(shift > 0)
          chord_invert(notes.to_a[1..-1] + [notes.to_a[0]+12], shift-1)
        elsif(shift < 0)
          chord_invert((notes.to_a[0..-2] + [notes.to_a[-1]-12]).sort, shift+1)
        else
          notes.ring
        end
      end
      doc name:          :chord_invert,
      introduced:    Version.new(2,6,0),
      summary:       "Chord inversion",
      doc:           "Given a set of notes, apply a number of inversions indicated by the `shift` parameter. Inversions being an increase to notes if `shift` is positive or decreasing the notes if `shift` is negative.

An inversion is simply rotating the chord and shifting the wrapped notes up or down an octave. For example, consider the chord :e3, :minor - `(ring 52, 55, 59)`. When we invert it once, we rotate the notes around to `(ring 55, 59, 52)`. However, because note 52 is wrapped round, it's shifted up an octave (12 semitones) so the actual first inversion of the chord :e3, :minor is `(ring 55, 59, 52 + 12)` or `(ring 55, 59, 64)`.

Note that it's also possible to directly invert chords on creation with the `invert:` opt - `(chord :e3, :minor, invert: 2)`",
      args:          [[:notes, :list], [:shift, :number]],
      returns:        :ring,
      opts:          nil,
      accepts_block: false,
      examples:      ["
play (chord_invert (chord :A3, \"M\"), 0) #No inversion     - (ring 57, 61, 64)
sleep 1
play (chord_invert (chord :A3, \"M\"), 1) #First inversion  - (ring 61, 64, 69)
sleep 1
play (chord_invert (chord :A3, \"M\"), 2) #Second inversion - (ring 64, 69, 73)
"]


      # keep for backwards compatibility
      def invert_chord(*args)
        chord_invert(*args)
      end




      def scale_names
        Scale::SCALE.keys.sort.ring
      end
      doc name:          :scale_names,
      introduced:    Version.new(2,6,0),
      summary:       "All scale names",
      doc:           "Returns a ring containing all scale names known to Sonic Pi",
      args:          [],
      opts:          nil,
      accepts_block: false,
      memoize:       true,
      examples:      ["puts scale_names #=>  prints a list of all the scales"]


      def chord_names
        Chord::CHORD_NAMES.ring
      end
      doc name:          :chord_names,
      introduced:    Version.new(2,6,0),
      summary:       "All chord names",
      doc:           "Returns a ring containing all chord names known to Sonic Pi",
      args:          [],
      opts:          nil,
      accepts_block: false,
      memoize:       true,
      examples:      ["puts chord_names #=>  prints a list of all the chords"]

    end
  end
end
