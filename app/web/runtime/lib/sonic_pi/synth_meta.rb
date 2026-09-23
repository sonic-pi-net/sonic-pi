# SPDX-License-Identifier: AGPL-3.0-or-later
# A synth's metadata: the file beside its .scsyndef (whoosh.scsyndef, whoosh.json) that makes it a standard Sonic Pi
# synth — its opts' defaults and ranges, which slide, which scale with the tempo, which track another opt, and how the
# GUI shows it. The built-in synths are described the same way (web/data/synth-meta.json, from native's SynthInfo), so
# one loaded with its metadata is played, checked and shown as they are. The page fetches the file before the run that
# loads its synthdef (live-worker.js) and hands it here (sp_install_synth); this turns it into the table entry
# SonicPi::Data::SYNTHS holds for a built-in.
#
#   {
#     "name": "whoosh",                        the synthdef's own name
#     "title": "Whoosh",                       what people call it
#     "summary": "A breathy sweep.",           one sentence, where there is room for no more
#     "description": "A breathy sweep of …",   the whole of it, for its docs page (plain text, `code` as code)
#     "opts": {                                each opt: its default, its type and range, what it does
#       "note":        { "default": 52, "type": "note", "range": [0, 127], "doc": "The note to play." },
#       "cutoff":      { "default": 100, "type": "number", "range": [0, 130], "slidable": true, "doc": "…" },
#       "release":     { "default": 1, "type": "time", "range": [0, null], "bpm_scale": true, "doc": "…" },
#       "decay_level": { "default": "sustain_level", "doc": "…" },
#       "wave":        { "default": 0, "type": "choice", "options": [0, 1, 2], "doc": "0 saw, 1 pulse, 2 triangle" },
#       "norm":        { "default": 0, "type": "switch", "doc": "…" }
#     },
#     "gui": { "basic": ["cutoff", "res"] }    the two of its own a GUI shows first
#   }
#
# An opt's type: "number" (the default), "note" (a MIDI number or a note's name, :c4), "time" (in beats), "choice"
# (one of its options), "switch" (0 off, 1 on). A range is [min, max], null for no max; a default naming another opt
# tracks it.
module SonicPi
  module SynthMeta
    SLIDE_SHAPES = [0, 1, 2, 3, 4, 6, 7, 8]

    class MetaError < StandardError; end

    module_function

    # The metadata's JSON, as Ruby values (Hash, Array, String, Numeric, true, false, nil). mruby has no JSON reader of
    # its own; this is one for the subset JSON is.
    def parse(text)
      @s = text.to_s
      @i = 0
      v = value
      space
      raise MetaError, "the metadata has more after its end, at character #{@i}" if @i < @s.size
      v
    end

    def space = (@i += 1 while @i < @s.size && " \t\r\n".include?(@s[@i]))

    def value
      space
      c = @s[@i]
      case c
      when "{" then object
      when "[" then array
      when '"' then string
      when "t" then word("true", true)
      when "f" then word("false", false)
      when "n" then word("null", nil)
      else number
      end
    end

    def word(w, v)
      raise MetaError, "the metadata is not JSON near character #{@i}" unless @s[@i, w.size] == w
      @i += w.size
      v
    end

    def object
      @i += 1
      h = {}
      space
      if @s[@i] == "}" then @i += 1; return h end
      loop do
        space
        raise MetaError, "an object's key must be a string, near character #{@i}" unless @s[@i] == '"'
        k = string
        space
        raise MetaError, "expected : near character #{@i}" unless @s[@i] == ":"
        @i += 1
        h[k] = value
        space
        if @s[@i] == "," then @i += 1; next end
        if @s[@i] == "}" then @i += 1; return h end
        raise MetaError, "expected , or } near character #{@i}"
      end
    end

    def array
      @i += 1
      a = []
      space
      if @s[@i] == "]" then @i += 1; return a end
      loop do
        a << value
        space
        if @s[@i] == "," then @i += 1; next end
        if @s[@i] == "]" then @i += 1; return a end
        raise MetaError, "expected , or ] near character #{@i}"
      end
    end

    ESCAPES = { '"' => '"', "\\" => "\\", "/" => "/", "b" => "\b", "f" => "\f", "n" => "\n", "r" => "\r", "t" => "\t" }
    def string
      @i += 1
      out = String.new
      while @i < @s.size
        c = @s[@i]
        if c == '"' then @i += 1; return out end
        if c == "\\"
          e = @s[@i + 1]
          if e == "u"
            out << [@s[@i + 2, 4].to_i(16)].pack("U")
            @i += 6
          else
            out << (ESCAPES[e] || e.to_s)
            @i += 2
          end
        else
          out << c
          @i += 1
        end
      end
      raise MetaError, "a string in the metadata is not closed"
    end

    def number
      start = @i
      @i += 1 while @i < @s.size && "+-0123456789.eE".include?(@s[@i])
      t = @s[start...@i]
      raise MetaError, "the metadata is not JSON near character #{start}" if t.empty?
      t.match?(/[.eE]/) ? t.to_f : t.to_i
    end

    # A synth's metadata, installed as a synth the runtime knows: played with its defaults, its opts checked in Safe
    # mode against their ranges, its slides and tempo-scaled times as a built-in's are. Its name, or an error that
    # says what in the file is wrong.
    def install(json)
      meta = parse(json)
      raise MetaError, "a synth's metadata is an object: { \"name\": …, \"opts\": { … } }" unless meta.is_a?(Hash)
      name = meta["name"].to_s
      raise MetaError, "the metadata needs the synth's name, as its synthdef has it (\"name\": \"whoosh\")" unless name.match?(/\A[A-Za-z0-9_\-]+\z/)
      opts = meta["opts"] || {}
      raise MetaError, "\"opts\" is an object of the synth's opts: { \"cutoff\": { \"default\": 100 } }" unless opts.is_a?(Hash)
      defaults = {}
      rules = {}
      slides = []
      bpm = []
      opts.each do |k, o|
        raise MetaError, "opt #{k} is an object: { \"default\": … }" unless o.is_a?(Hash)
        raise MetaError, "opt #{k} is not a name an opt can have" unless k.to_s.match?(/\A[a-z_][a-z0-9_]*\z/)
        key = k.to_sym
        d = o["default"]
        # a default naming another opt tracks it (decay_level: :sustain_level), as native's defaults do
        defaults[key] = d.is_a?(String) ? d.to_sym : d
        rules[key] = rules_for(k, o)
        bpm << key if o["bpm_scale"] || o["type"] == "time"   # a time is in beats: the tempo scales it
        next unless o["slidable"]
        slide = :"#{k}_slide"
        defaults[slide] = 0
        defaults[:"#{k}_slide_shape"] = 1
        defaults[:"#{k}_slide_curve"] = 0
        rules[slide] = [{ kind: :positive, min: 0, incl: true }]
        rules[:"#{k}_slide_shape"] = [{ kind: :one_of, options: SLIDE_SHAPES }]
        slides << slide
        bpm << slide
      end
      opts.each_key { |k| d = defaults[k.to_sym]; raise MetaError, "opt #{k} tracks #{d}, which is not one of the synth's opts" if d.is_a?(Symbol) && !defaults.key?(d) }
      rules.delete_if { |_, r| r.empty? }
      SonicPi::Data::SYNTHS[name.to_sym] = {
        scsynth_name: name, defaults: defaults, bpm_scale_args: bpm, slide_args: slides, rules: rules,
        kill_delay: (meta["kill_delay"] || 1), metadata: true,
      }
      name.to_sym
    end

    TYPES = %w[number note time choice switch]

    def rules_for(k, o)
      out = []
      type = o["type"]
      raise MetaError, "opt #{k}'s type is one of #{TYPES.join(", ")}: got #{type.inspect}" unless type.nil? || TYPES.include?(type)
      return [{ kind: :one_of, options: [0, 1] }] if type == "switch"
      return out if type == "note"   # a MIDI number or a note's name (:c4): Sonic Pi checks neither against a range
      if o["options"].is_a?(Array)
        out << { kind: :one_of, options: o["options"] }
      elsif o["range"].is_a?(Array)
        lo, hi = o["range"]
        raise MetaError, "opt #{k}'s range is [min, max] (null for no max): got #{o["range"].inspect}" unless lo.is_a?(Numeric) && (hi.nil? || hi.is_a?(Numeric))
        out << (hi ? { kind: :between, min: lo, max: hi, incl: true } : { kind: (lo == 0 ? :positive : :min), min: lo, incl: true })
      end
      out
    end
  end
end
