# SPDX-License-Identifier: AGPL-3.0-or-later
# The rest of Sonic Pi v5's language, ported from its lang/core.rb,
# western_theory.rb, sound.rb and midi.rb: nodes that can be controlled and
# killed, time state (set and get), MIDI out, rings and lists, theory, the
# current_* getters, tunings, asserts. Each verb follows the original line
# for line where a program can see the difference; specs/ pins them against
# the oracle. What the web build cannot have says so when called.
module SonicPi
  module Lang
    module Core
    end
  end

  class Scale
    class InvalidDegreeError < ArgumentError; end
  end

  # What play gives back for a list of notes: its synths, controlled as one.
  class ChordGroup
    attr_reader :nodes, :notes, :info

    def initialize(nodes, notes, info)
      @nodes = nodes
      @notes = notes
      @info = info
    end

    def id = @nodes.empty? ? nil : @nodes[0].id
    def name = @nodes.empty? ? nil : @nodes[0].name
    def sub_nodes = @nodes
    def to_s = "#<SonicPi::ChordGroup @subnodes=#{@nodes.map(&:to_s)}>"
    def inspect = to_s
  end

  # Sonic Pi's vector: a ring that does not wrap.
  class Vector < Ring
    def ___sp_vector_name = "vector"
    def map_index(idx) = idx
    def ring = self
  end

  # Sonic Pi's map: an immutable hash that prints like its other values.
  class SPMap
    attr_reader :map

    def initialize(*args)
      @map = if args.length == 1 && args[0].is_a?(SPMap)
               args[0].map
             elsif args.length == 1 && args[0].is_a?(Hash)
               args[0].dup
             else
               raise ArgumentError, "odd number of arguments for Hash" if args.size.odd?   # as Hash[*args] says
               h = {}
               i = 0
               while i < args.size
                 h[args[i]] = args[i + 1]
                 i += 2
               end
               h
             end
      @map.freeze
    end

    def [](k) = @map[k]
    def fetch(*args, &blk) = @map.fetch(*args, &blk)
    def key?(k) = @map.key?(k)
    def has_key?(k) = @map.key?(k)
    def keys = @map.keys
    def values = @map.values
    def each(&blk) = @map.each(&blk)
    def size = @map.size
    def length = @map.size
    def empty? = @map.empty?
    def to_h = @map.dup
    def merge(*others, &blk)
      h = @map.dup
      others.each { |o| (o.is_a?(SPMap) ? o.map : o).each { |k, v| h[k] = blk && h.key?(k) ? blk.call(k, h[k], v) : v } }
      SPMap.new(h)
    end
    # the rest of native's map (core.rb SPMap), written out where mruby's Hash has no such method: a map where
    # native gives a map, the Hash's own answer elsewhere
    def select(&blk) = SPMap.new(@map.select(&blk))
    def filter(&blk) = select(&blk)
    def reject(&blk) = SPMap.new(@map.reject(&blk))
    def any?(&blk) = blk ? @map.any? { |k, v| blk.call(k, v) } : !@map.empty?
    def dig(*keys)
      v = @map[keys[0]]
      keys.size == 1 || v.nil? ? v : v.dig(*keys[1..])
    end
    def slice(*keys) = SPMap.new(keys.each_with_object({}) { |k, h| h[k] = @map[k] if @map.key?(k) })
    def include?(k) = @map.key?(k)
    def has_value?(v) = @map.values.include?(v)
    def each_pair(&blk) = @map.each(&blk)
    def each_key(&blk) = @map.keys.each(&blk)
    def invert = SPMap.new(@map.each_with_object({}) { |(k, v), h| h[v] = k })
    def key(v) = (@map.find { |_, x| x == v } || [])[0]
    def compact = SPMap.new(@map.reject { |_, v| v.nil? })
    def fetch_values(*keys, &blk) = keys.map { |k| @map.fetch(k, &blk) }
    def assoc(k) = @map.key?(k) ? [k, @map[k]] : nil
    def rassoc(v) = (pair = @map.find { |_, x| x == v }) ? SonicPi::Ring.new(pair) : nil
    def flatten(depth = 1) = @map.to_a.flatten(depth)
    def eql?(other) = @map.eql?(other)
    def <=(other) = @map.all? { |k, v| other.map.key?(k) && other.map[k] == v }
    def <(other) = self <= other && @map.size < other.map.size
    def >=(other) = other <= self
    def >(other) = other < self
    def ==(other) = other.is_a?(SPMap) && other.map == @map

    def inspect
      return "(map)" if @map.empty?
      parts = @map.map { |k, v| k.is_a?(Symbol) ? "#{k}: #{SonicPi.log_inspect(v)}" : "#{k.inspect} => #{SonicPi.log_inspect(v)}" }
      "(map #{parts.join(', ')})"
    end
    def to_s = inspect

    def sp_log_inspect
      return "(map)" if @map.empty?
      longest = @map.keys.map { |k| k.to_s.size }.max + 2
      s = "(map "
      @map.each do |k, v|
        if k.is_a?(Symbol)
          s += "#{"#{k}:".ljust(longest)} #{SonicPi.log_inspect(v)},\n       "
        else
          s += "#{k.inspect.ljust(longest)} => #{SonicPi.log_inspect(v)},\n       "
        end
      end
      s = s.strip
      s = s[0, s.size - 1] if s.end_with?(",")
      s + ")"
    end
  end

  # What note_info gives: a note's name, octave and number.
  class NoteInfo
    NAMES = { 0 => :C, 1 => :Cs, 2 => :D, 3 => :Eb, 4 => :E, 5 => :F, 6 => :Fs, 7 => :G, 8 => :Ab, 9 => :A, 10 => :Bb, 11 => :B }
    attr_reader :pitch_class, :octave, :interval, :midi_note, :midi_string
    def to_h = { pitch_class: @pitch_class, octave: @octave, interval: @interval, midi_note: @midi_note, midi_string: @midi_string }

    def initialize(n, o = nil)
      if n.is_a?(Numeric)
        o = (n / 12).to_i - 1
        n = NAMES[(n % 12).to_i]
      end
      orig = n
      s = n.to_s
      s = s[1..] if s.start_with?(":")
      letter = s[0].to_s
      base = Note::INTERVALS[letter.downcase] or raise InvalidNoteError, "Invalid note: #{orig.inspect}"
      i = 1
      mod = ""
      c = s[1].to_s
      if "sSbBfF".include?(c) && !c.empty?
        mod = c.downcase
        i = 2
      end
      rest = s[i..].to_s
      digits = rest.start_with?("-") ? rest[1..] : rest
      raise InvalidNoteError, "Invalid note: #{orig.inspect}" unless digits.each_char.all? { |d| d >= "0" && d <= "9" }
      raise InvalidNoteError, "Invalid note: #{orig.inspect}" if rest == "-"
      @pitch_class = "#{letter.upcase}#{mod == 'f' ? 'b' : mod}".to_sym
      if o
        raise InvalidOctaveError, "Invalid octave: #{o.inspect}, expecting a whole number such as 3 or 4!" unless o.is_a?(Numeric) && o == o.to_i
        @octave = o.to_i
      else
        @octave = rest.empty? ? Note::DEFAULT_OCTAVE : rest.to_i
      end
      @interval = base + (mod == "s" ? 1 : (mod.empty? ? 0 : -1))
      @midi_note = (@octave * 12) + @interval + 12
      @midi_string = "#{@pitch_class}#{@octave}"
    end

    def to_s = "#<SonicPi::Note :#{@midi_string}>"
    def inspect = to_s
  end

  # Sonic Pi's tunings, as its tuning.rb computes them.
  class Tuning
    TUNINGS = {
      just: [1, 16.0 / 15.0, 9.0 / 8.0, 6.0 / 5.0, 5.0 / 4.0, 4.0 / 3.0, 45.0 / 32.0, 3.0 / 2.0, 8.0 / 5.0, 5.0 / 3.0, 9.0 / 5.0, 15.0 / 8.0],
      pythagorean: [1.0 / 1, 2187.0 / 2048, 9.0 / 8, 19683.0 / 16384, 81.0 / 64, 4.0 / 3, 729.0 / 512, 3.0 / 2, 6561.0 / 4096, 27.0 / 16, 59049.0 / 32768, 243.0 / 128],
      meantone: [1.00000, 1.0449, 1.1180, 1.1963, 1.2500, 1.3375, 1.3975, 1.4953, 1.5625, 1.6719, 1.7889, 1.8692],
    }
    @cache = {}

    def self.midi_to_hz(n) = (440.0 * (2 ** ((n - 69) / 12.0))).round(9)
    def self.hz_to_midi(freq) = ((12 * (Math.log(freq * 0.0022727272727) / Math.log(2))) + 69).round(9)

    def self.resolve(note, system, fundamental)
      return note if system == :equal
      ratios = TUNINGS[system] or raise "Unknown tuning system #{system.inspect}"
      offset = Note.midi(fundamental) - 60
      notes = (@cache[[system, offset]] ||= (0..150).map { |m| hz_to_midi(midi_to_hz(m - ((m - offset) % 12)) * ratios[(m - offset) % 12]) })
      if note.is_a?(Float)
        above = notes[note.ceil]
        below = notes[note.floor]
        hz_to_midi(midi_to_hz(below) + ((midi_to_hz(above) - midi_to_hz(below)) * (note - note.floor)))
      else
        notes[note]
      end
    end
  end

  class << self
    attr_accessor :external_synthdefs   # name → URL, what load_synthdef has loaded this session
    attr_accessor :live_audio_nodes     # live_audio's synths by name, across runs
    attr_writer :link_bpm
    def link_bpm = (@link_bpm ||= 60.0)
  end

  class Language
    # What the web build does not have: never (a browser cannot), or not yet — said as the docs say it
    # (scripts/gen-runtime-support.mjs, where the same reasons are kept)
    NOT_ON_WEB = {
      load_buffer: [:never, "there are no files on the web"], run_file: [:never, "there are no files on the web"],
      eval_file: [:never, "there are no files on the web"],
      link_audio: [:never, "Link Audio shares audio between apps over the local network, which a browser cannot join"],
      buffer: [:later, "named buffers are not built yet"],
      midi_sync: [:later, "following an incoming MIDI clock is not built yet"],
      midi_clock_sources: [:later, "following an incoming MIDI clock is not built yet"],
    }
    NOT_ON_WEB.each do |name, (kind, why)|
      define_method(name) { |*_args, &_blk| raise "#{name} is #{kind == :never ? 'never on the web' : 'not on the web yet'}: #{why}" }
    end

    # ── Live audio ────────────────────────────────────────────────────────
    # native's live_audio: the sound card's input as a named synth, one a name, playing until stopped and kept across
    # runs; called again inside another with_fx it moves into it (Scheduler#live_move), and `live_audio :name, :stop`
    # stops it. On the web the input is the browser's: the page asks for the microphone (or line in) the first time.
    def live_audio(*params)
      args, opts = split_params_and_merge_opts_array(params)
      raise "live_audio requires a name" if args.empty?
      id = args[0]
      lives = (SonicPi.live_audio_nodes ||= {})
      p = __p
      if args.size > 1 && (args[1].nil? || args[1] == :stop)
        node = lives.delete(id)
        @sched.record_kill(p, node) if node && __live_audio_on?(node, p)
        return nil
      end
      opts = opts.dup
      stereo = truthy?(opts.delete(:stereo))
      input = (opts[:input] || 1).to_i
      raise "live_audio's input: must be 1 or more, got #{opts[:input].inspect}" if input < 1
      synth = stereo ? "sonic-pi-live_audio_stereo" : "sonic-pi-live_audio_mono"
      __log("live_audio #{id.inspect}, #{arg_h_pp(opts)}") unless __local(:synth_silent)
      @sched.record_audio_in(p, input + (stereo ? 1 : 0))
      place = @sched.live_place(p)
      node = lives[id]
      if node && __live_audio_on?(node, p) && node.name == synth
        return node if node.frame_place.equal?(place)   # where it is already: as it is (native's)
        @sched.live_move(p, node, place)
        node.ends_at = p.start + p.time   # the fx it leaves may end now; the synth goes on in its new place
        moved = SynthNode.new(synth, node.args, nil, node.id, node.ref)
        moved.ends_at = LIVE_AUDIO_FOREVER
        moved.frame_place = place
        @sched.track(p, moved)
        return lives[id] = moved
      end
      @sched.record_kill(p, node) if node && __live_audio_on?(node, p)   # mono to stereo, or back: a new synth
      args_h = {}
      opts.each { |k, v| args_h[k] = v.is_a?(Numeric) ? v.to_f : v }
      id_n = @sched.next_node_id
      recorded = {}
      args_h.each { |k, v| recorded[k.to_s] = v }
      @sched.record_event(p, { synth: synth, args: recorded, now: false }, id_n)
      node = SynthNode.new(synth, args_h, nil, id_n, { t: p.time.round(6), thread: p.id, sa: p.sched_ahead })
      node.ends_at = LIVE_AUDIO_FOREVER
      node.frame_place = place
      @sched.track(p, node)
      __set_local(:last_node, node)
      lives[id] = node
    end
    LIVE_AUDIO_FOREVER = 1.0e12
    def __live_audio_on?(node, p) = node.ends_at && node.ends_at > p.start + p.time

    # ── External synthdefs ────────────────────────────────────────────────
    # Native loads a .scsyndef from a path; the web has no files, so a synthdef comes from a URL, which the page
    # fetches and hands to SuperSonic (sonic_pi.js Bridge#synthDef). Its name is the file's (whoosh.scsyndef names
    # :whoosh, as SuperCollider's writeDefFile names it). Triggering one needs use_external_synths true, which the
    # Preferences' "Enable external synths and FX" sets for every run, as native's does.
    EXTERNAL_URL = %r{\A(?:https?:)?//|\A/|\Adata:}

    def load_synthdef(url = nil)
      raise "load_synthdef needs the URL of a synth design, e.g. load_synthdef \"https://example.com/whoosh.scsyndef\"" if url.nil? || url.to_s.empty?
      url = url.to_s
      unless url.match?(EXTERNAL_URL)
        raise "load_synthdef on the web takes a URL, not a path: there are no files on the web. Put the .scsyndef somewhere the web can reach (a GitHub raw link works) and pass its https:// address. Got: #{url.inspect}"
      end
      file = url.split(/[?#]/, 2)[0].split("/").last.to_s
      unless file.downcase.end_with?(".scsyndef")
        raise "load_synthdef needs a .scsyndef file (to load several, pass load_synthdefs a list of their URLs). Got: #{url.inspect}"
      end
      name = file[0...-9]
      (SonicPi.external_synthdefs ||= {})[name.to_sym] = url
      @sched.record_synthdef_load(__p, url)
      __log("Loaded synthdef: #{url}")
      name.to_sym
    end

    def load_synthdefs(urls = nil)
      list = urls.is_a?(Array) || urls.is_a?(SonicPi::Ring) ? urls.to_a : [urls]
      if list.size == 1 && !list[0].to_s.downcase.split(/[?#]/, 2)[0].end_with?(".scsyndef") && list[0].to_s.match?(EXTERNAL_URL)
        raise "load_synthdefs on the web takes a list of .scsyndef URLs: a web address cannot be listed like a folder. Got: #{list[0].inspect}"
      end
      list.map { |u| load_synthdef(u) }
    end

    def use_external_synths(v, &block)
      raise "use_external_synths does not work with a do/end block. Perhaps you meant with_external_synths" if block
      __set_local(:external_synths, truthy?(v))
    end

    def with_external_synths(v, &block)
      raise "with_external_synths requires a do/end block. Perhaps you meant use_external_synths" unless block
      __with_local(:external_synths, truthy?(v)) { block.call }
    end

    # a synth the runtime does not know, played by its own name: no defaults, no checks, its opts as given (native's)
    def __external_synth_info(name)
      { scsynth_name: name.to_s, defaults: {}, validations: {}, aliases: {}, slide_args: [], bpm_scale_args: [], kill_delay: 1, external: true }
    end

    # ── Nodes ─────────────────────────────────────────────────────────────

    def __node?(x) = x.is_a?(SynthNode) || x.is_a?(ChordGroup) || x.is_a?(BlankNode)
    # a node's pause (false) or run again (true), as native's node.pause and node.run; and whether it has ended
    def __node_run(node, on) = @sched.record_run(__p, node, on)
    def __node_ended?(node) = !!(node.ends_at && __p && __p.start + __p.time >= node.ends_at)

    def control(*args) = __control(args, true)

    # control, logged (the control fn) or not (a node's own control, as native's node.ctl, which goes straight to
    # the engine)
    def __control(args, logged)
      node = __node?(args.first) ? args.shift : __local(:last_node)
      return nil if node.nil?
      args_h = resolve_synth_opts_hash_or_array(args).dup
      return nil unless __should_trigger?(args_h)
      return node if node.is_a?(BlankNode)
      info = node.info
      defaults = info ? info[:defaults] : {}
      if info
        __alias_opts!(args_h, info)
        __resolve_midi_args!(args_h, info)
        __add_slide_times!(args_h, info)
        mul = __p.sleep_mul
        info[:bpm_scale_args].each { |a| args_h[a] = args_h[a] * mul if args_h[a].is_a?(Numeric) }
      end
      return node if __out_of_time?("control node #{node.id}, #{arg_h_pp(args_h)}")
      if node.is_a?(ChordGroup)
        note = args_h.delete(:note)
        notes = args_h.delete(:notes)
        notes = note if note && !notes
        __normalise_args!(args_h, defaults)
        if notes
          notes = [notes] unless is_list_like?(notes)
          args_h[:notes] = notes.map { |n| __resolve_note(n, args_h) }
        end
        __control_chord(node, args_h)
      else
        args_h[:note] = __resolve_note(args_h[:note], args_h) if args_h[:note]
        __normalise_args!(args_h, defaults)
        @sched.record_control(__p, node, __recorded_args(args_h))
      end
      __log("control node #{node.id}, #{arg_h_pp(args_h)}") if logged && !__local(:synth_silent)
      node
    end

    def __control_chord(group, args_h)
      notes = args_h[:notes]
      return unless notes && notes.respond_to?(:each_with_index)
      h = args_h.dup
      h[:amp] = h[:amp].to_f / group.nodes.size if h[:amp] && !group.nodes.empty?
      h.delete(:notes)
      ring = notes.to_a.ring
      group.nodes.each_with_index do |sn, idx|
        @sched.record_control(__p, sn, __recorded_args(h.merge({ note: ring[idx] })))
      end
    end

    def __recorded_args(args_h)
      recorded = {}
      args_h.each { |k, v| recorded[k.to_s] = v }
      recorded
    end

    def kill(node, logged = true)
      return nil if node.nil?
      if node.is_a?(SynthNode)
        @sched.record_kill(__p, node)
        node.killed!
      end
      if logged && !__local(:synth_silent)
        __log(node.is_a?(BlankNode) ? "not killing sound  (already killed)" : "killing sound #{node.id}")
      end
      nil
    end

    # ── Time state ────────────────────────────────────────────────────────

    class TimeStateLookup
      def initialize(blk) = (@blk = blk)
      def [](*args) = @blk.call(*args)
    end

    def set(k, val) = __cueset(k, val, "set")

    def __cueset(k, val, prefix)
      p = __p
      address = __cue_address(k, prefix)
      ev = @sched.cue(p, p.priority, address, val)
      p.state_cache.unshift([address, ev])
      unless __local(:suppress_cue_logging)
        if val.nil?
          __log("#{prefix} #{k.inspect}")
        elsif is_list_like?(val)
          __log("#{prefix} #{k.inspect}, #{SonicPi.log_inspect(val)}")
        else
          __log("#{prefix} #{k.inspect}, #{SonicPi.log_inspect(val)}")
        end
      end
      val
    end

    def get(*args)
      return TimeStateLookup.new(lambda { |*a| get(*a) }) if args.empty?
      params, opts = split_params_and_merge_opts_array(args)
      default = params[1] || opts[:default]
      p = __p
      # a get from a time_warp into the future would bring the future back to now (native's TimingError)
      if p.in_time_warp && p.time_warp_start && p.time_warp_start < p.time
        raise SonicPi::Lang::Core::TimingError, "Sadly, you may not time_warp into the future to call get, then bring the result back in time to now."
      end
      ev = __get_event(params[0])
      ev ? ev.val : default
    end

    def __get_event(k)
      p = __p
      addresses = __sync_addresses(k)
      hit = p.state_cache.find { |(a, _)| addresses.include?(a) }
      return hit[1] if hit
      @sched.get_event(p, addresses)
    end

    def use_cue_logging(v, &block)
      raise "use_cue_logging does not work with a do/end block. Perhaps you meant with_cue_logging" if block
      __set_local(:suppress_cue_logging, !v)
    end

    def with_cue_logging(v, &block)
      raise ArgumentError, "with_cue_logging requires a do/end block. Perhaps you meant use_cue_logging" unless block
      __with_local(:suppress_cue_logging, !v) { block.call }
    end

    # ── MIDI out ──────────────────────────────────────────────────────────

    def current_midi_defaults = __local(:midi_defaults) || {}
    def current_midi_channels = __local(:midi_channel) || ["*"]
    def current_midi_ports = __local(:midi_ports) || ["*"]

    def use_midi_defaults(*args, &block)
      raise "use_midi_defaults does not work with a block. Perhaps you meant with_midi_defaults" if block
      __set_local(:midi_defaults, SPMap.new(resolve_synth_opts_hash_or_array(args)))
    end

    def with_midi_defaults(*args, &block)
      raise "with_midi_defaults must be called with a do/end block" unless block
      __with_local(:midi_defaults, SPMap.new(resolve_synth_opts_hash_or_array(args))) { block.call }
    end

    def use_merged_midi_defaults(*args, &block)
      raise "use_merged_midi_defaults does not work with a block. Perhaps you meant with_midi_defaults" if block
      __set_local(:midi_defaults, SPMap.new((__local(:midi_defaults) || {}).merge(resolve_synth_opts_hash_or_array(args))))
    end

    def with_merged_midi_defaults(*args, &block)
      raise "with_merged_midi_defaults must be called with a do/end block" unless block
      merged = SPMap.new((__local(:midi_defaults) || {}).merge(resolve_synth_opts_hash_or_array(args)))
      __with_local(:midi_defaults, merged) { block.call }
    end

    def use_midi_logging(v, &block)
      raise "use_midi_logging does not work with a do/end block. Perhaps you meant with_midi_logging" if block
      __set_local(:suppress_midi_logging, !v)
    end

    def with_midi_logging(v, &block)
      raise ArgumentError, "with_midi_logging requires a do/end block. Perhaps you meant use_midi_logging" unless block
      __with_local(:suppress_midi_logging, !v) { block.call }
    end

    def __mclamp(v, lo, hi) = v < lo ? lo : (v > hi ? hi : v)
    def __midi_args(args)
      params, opts = split_params_and_merge_opts_array(args)
      [params, current_midi_defaults.merge(opts)]
    end
    def __midi_message(m) = (__log(m) unless __local(:suppress_midi_logging))
    def pp_el_or_list(l) = l.size == 1 ? SonicPi.log_inspect(l[0]) : SonicPi.log_inspect(l.to_a)
    def __num_s(v) = v.is_a?(Float) ? SonicPi::FloatFormat.to_s(v) : v.to_s

    def __resolve_midi_channels(opts)
      channels = opts[:channel] || opts[:chan] || current_midi_channels
      return ["*"] if channels == "*"
      if is_list_like?(channels)
        res = []
        channels.each do |c|
          return ["*"] if c == "*"
          res << __mclamp(c.to_i, 1, 16)
        end
        return res.uniq
      end
      [__mclamp(channels.to_i, 1, 16)]
    end

    def __resolve_midi_ports(opts)
      ports = opts[:port] || current_midi_ports
      if is_list_like?(ports)
        return ["*"] if ports.to_a.include?("*")
        return ports.to_a
      end
      [ports]
    end

    def __resolve_midi_velocity(vel, opts)
      if (v = opts[:velocity] || opts[:vel] || vel)
        __mclamp(note(v).round, 0, 127)
      elsif (v = opts[:velocity_f] || opts[:vel_f])
        __mclamp((v.to_f * 127).round, 0, 127)
      else
        127
      end
    end

    def __resolve_midi_val(val, opts)
      if (v = opts[:value] || opts[:val] || val)
        __mclamp(note(v).round, 0, 127)
      elsif (v = opts[:value_f] || opts[:val_f])
        __mclamp((v.to_f * 127).round, 0, 127)
      else
        127
      end
    end

    def __resolve_midi_deltas(delta, opts)
      if (d = opts[:delta_midi])
        delta_midi = __mclamp(d.round, 0, 16383)
        [delta_midi / 16383.0, delta_midi]
      elsif (d = opts[:delta] || opts[:val_f] || delta)
        [__mclamp(d.to_f, 0, 1), (d * 16383).round]
      else
        [0.5, 8192]
      end
    end

    def __midi_send(path, *args) = @sched.record_midi(__p, path, args)

    def __midi_send_pc(path, port, chan, a0, a1 = nil)
      chan = -1 if chan == "*"
      a1.nil? ? __midi_send(path, port, chan, a0) : __midi_send(path, port, chan, a0, a1)
    end

    def __midi_each(ports, channels)
      ports.each { |p| channels.each { |c| yield p, c } }
    end

    def midi_note_on(*args)
      params, opts = __midi_args(args)
      n, vel = *params
      return __midi_message("midi_note_on :rest") && nil if rest?(n)
      if truthy?(opts.fetch(:on, 1))
        n = __mclamp(__resolve_note(n, opts).round, 0, 127)
        channels = __resolve_midi_channels(opts)
        ports = __resolve_midi_ports(opts)
        vel = __resolve_midi_velocity(vel, opts)
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/note_on", p, c, n, vel) }
        __midi_message "midi_note_on #{n}, #{vel}, channel: #{pp_el_or_list(channels)}, port: #{pp_el_or_list(ports)}"
      else
        __midi_message "midi_note_on :rest, on: 0"
      end
      nil
    end

    def midi_note_off(*args)
      params, opts = __midi_args(args)
      n, vel = *params
      return __midi_message("midi_note_off :rest") && nil if rest?(n)
      n = __resolve_note(n, opts)
      if truthy?(opts.fetch(:on, 1))
        channels = __resolve_midi_channels(opts)
        ports = __resolve_midi_ports(opts)
        vel = __resolve_midi_velocity(vel, opts)
        n = __mclamp(note(n).round, 0, 127)
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/note_off", p, c, n, vel) }
        __midi_message "midi_note_off #{n}, #{vel}, channel: #{pp_el_or_list(channels)}, port: #{pp_el_or_list(ports)}"
      else
        __midi_message "midi_note_off :rest, on: 0"
      end
      nil
    end

    def midi_poly_pressure(*args)
      params, opts = __midi_args(args)
      control_num, val = *params
      return __midi_message("midi_poly_pressure :rest") && nil if rest?(control_num)
      if truthy?(opts.fetch(:on, 1))
        channels = __resolve_midi_channels(opts)
        ports = __resolve_midi_ports(opts)
        val = __resolve_midi_val(val, opts)
        control_num = __mclamp(note(control_num).round, 0, 127)
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/aftertouch", p, c, control_num, val) }
        __midi_message "midi_poly_pressure #{control_num}, #{val}, port: #{pp_el_or_list(ports)}, channel: #{pp_el_or_list(channels)}"
      else
        __midi_message "midi_poly_pressure :rest, on: 0"
      end
      nil
    end

    def midi_cc(*args)
      params, opts = __midi_args(args)
      control_num, val = *params
      return __midi_message("midi_cc :rest") && nil if rest?(control_num)
      if truthy?(opts.fetch(:on, 1))
        channels = __resolve_midi_channels(opts)
        ports = __resolve_midi_ports(opts)
        val = __resolve_midi_val(val, opts)
        control_num = __mclamp(note(control_num).round, 0, 127)
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/control_change", p, c, control_num, val) }
        __midi_message "midi_cc #{control_num}, #{val}, port: #{pp_el_or_list(ports)}, channel: #{pp_el_or_list(channels)}"
      else
        __midi_message "midi_cc :rest, on: 0"
      end
      nil
    end

    def midi_channel_pressure(*args)
      params, opts = __midi_args(args)
      pressure = params[0]
      return __midi_message("midi_channel_pressure :rest") && nil if params.size > 0 && rest?(pressure)
      if truthy?(opts.fetch(:on, 1))
        channels = __resolve_midi_channels(opts)
        ports = __resolve_midi_ports(opts)
        pressure = __resolve_midi_val(pressure, opts)
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/channel_pressure", p, c, pressure) }
        __midi_message "midi_channel_pressure #{pressure}, port: #{pp_el_or_list(ports)}, channel: #{pp_el_or_list(channels)}"
      else
        __midi_message "midi_channel_pressure :rest, on: 0"
      end
      nil
    end

    def midi_pitch_bend(*args)
      params, opts = __midi_args(args)
      delta = params[0]
      return __midi_message("midi_pitch_bend :rest") && nil if params.size > 0 && rest?(delta)
      if truthy?(opts.fetch(:on, 1))
        channels = __resolve_midi_channels(opts)
        ports = __resolve_midi_ports(opts)
        delta, delta_midi = __resolve_midi_deltas(delta, opts)
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/pitch_bend", p, c, delta_midi) }
        __midi_message "midi_pitch_bend #{__num_s(delta)}, delta_midi: #{delta_midi}, port: #{pp_el_or_list(ports)}, channel: #{pp_el_or_list(channels)}"
      else
        __midi_message "midi_pitch_bend :rest, on: 0"
      end
      nil
    end

    def midi_pc(*args)
      params, opts = __midi_args(args)
      program_num = params[0]
      return nil if program_num.nil?
      program_num = __mclamp(note(program_num).round, 0, 127)
      if truthy?(opts.fetch(:on, 1))
        channels = __resolve_midi_channels(opts)
        ports = __resolve_midi_ports(opts)
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/program_change", p, c, program_num) }
        __midi_message "midi_pc #{program_num}, port: #{pp_el_or_list(ports)}, channel: #{pp_el_or_list(channels)}"
      else
        __midi_message "midi_pc  #{program_num}, on: 0"
      end
      nil
    end

    def midi_raw(*args)
      params, opts = __midi_args(args)
      params = params.map { |x| x.to_f.round }
      ports = __resolve_midi_ports(opts)
      if truthy?(opts.fetch(:on, 1))
        ports.each { |p| __midi_send("/raw", p, *params) }
        __midi_message "midi_raw #{params.join(', ')}, port: #{pp_el_or_list(ports)}"
      else
        __midi_message "midi_raw #{params.join(', ')}, on: 0"
      end
      nil
    end

    def midi_sysex(*args)
      params, opts = __midi_args(args)
      params = params.map { |x| x.to_f.round }
      ports = __resolve_midi_ports(opts)
      raise "sysex messages must be at least 3 bytes long" if params.length < 3
      raise "sysex messages must start with 0xf0" unless params[0] == 0xf0
      raise "sysex messages must end with 0xf7" unless params[-1] == 0xf7
      if truthy?(opts.fetch(:on, 1))
        ports.each { |p| __midi_send("/raw", p, *params) }
        __midi_message "midi_sysex #{params.join(', ')}, port: #{pp_el_or_list(ports)}"
      else
        __midi_message "midi_sysex #{params.join(', ')}, on: 0"
      end
      nil
    end

    # The channel-mode messages: one control change each.
    def __midi_cc_verb(label, args, cc, value = 0)
      _params, opts = __midi_args(args)
      ports = __resolve_midi_ports(opts)
      channels = __resolve_midi_channels(opts)
      where = "port: #{pp_el_or_list(ports)}, channel: #{pp_el_or_list(channels)}"
      if truthy?(opts.fetch(:on, 1))
        __midi_each(ports, channels) { |p, c| __midi_send_pc("/control_change", p, c, cc, value) }
        __midi_message "#{label} #{where}"
      else
        __midi_message "#{label} #{where}, on: 0"
      end
      nil
    end

    def midi_all_notes_off(*args) = __midi_cc_verb("midi_all_notes_off", args, 123)
    def midi_sound_off(*args) = __midi_cc_verb("midi_sound_off", args, 120)
    def midi_local_control_off(*args) = __midi_cc_verb("midi_mode_local_control_off", args, 122, 0)
    def midi_local_control_on(*args) = __midi_cc_verb("midi_mode_local_control_on", args, 122, 127)

    def midi_reset(*args)
      params, opts = __midi_args(args)
      value = opts[:value] || opts[:val] || params[0] || 0
      __midi_cc_verb("midi_reset", args, 121, value)
    end

    def midi_mode(*args)
      params, opts = __midi_args(args)
      mode = opts[:mode] || params[0] || :omni_off
      case mode
      when :omni_off then __midi_cc_verb("midi_mode :omni_off,", args, 124, 0)
      when :omni_on then __midi_cc_verb("midi_mode :omni_on,", args, 125, 0)
      when :mono
        num_chans = opts[:num_chans] || 16
        __midi_cc_verb("midi_mode :mono, num_chans: #{num_chans},", args, 126, num_chans)
      when :poly then __midi_cc_verb("midi_mode :poly,", args, 127, 0)
      else raise "Unknown special mode for midi_mode: #{mode.inspect}. Expected one of: :omni_off, :omni_on, :mono or :poly."
      end
    end

    def __midi_transport(label, path, args)
      _params, opts = __midi_args(args)
      ports = __resolve_midi_ports(opts)
      if truthy?(opts.fetch(:on, 1))
        ports.each { |p| __midi_send(path, p) }
        __midi_message "#{label} port: #{pp_el_or_list(ports)}"
      else
        __midi_message "#{label} port: #{pp_el_or_list(ports)}, on: 0"
      end
      nil
    end

    def midi_clock_tick(*args) = __midi_transport("midi_clock_tick", "/clock", args)
    def midi_start(*args) = __midi_transport("midi_start", "/start", args)
    def midi_stop(*args) = __midi_transport("midi_stop", "/stop", args)
    def midi_continue(*args) = __midi_transport("midi_continue", "/continue", args)

    def midi_clock_beat(*args)
      params, opts = __midi_args(args)
      num_beats = opts[:duration] || params[0] || 1
      ports = __resolve_midi_ports(opts)
      if truthy?(opts.fetch(:on, 1))
        ports.each do |p|
          __midi_send("/clock_beat", p, __p.sleep_mul * 1000.0 * num_beats)
          __midi_message "midi_clock_beat port: #{pp_el_or_list(ports)}"
        end
      else
        __midi_message "midi_clock_beat port: #{pp_el_or_list(ports)}"
      end
      nil
    end

    def midi(*args)
      params, opts = __midi_args(args)
      n, vel = *params
      return __midi_message("midi :rest") && nil if rest?(n)
      n = __resolve_note(n, opts)
      on = opts.fetch(:on, 1)
      channels = __resolve_midi_channels(opts)
      ports = __resolve_midi_ports(opts)
      vel = __resolve_midi_velocity(vel, opts)
      sus = opts.fetch(:sustain, 1).to_f
      rel_vel = opts.fetch(:release_velocity, 127)
      n = __mclamp(n.round, 0, 127)
      where = "sustain: #{__num_s(sus)}, port: #{pp_el_or_list(ports)}, channel: #{pp_el_or_list(channels)}"
      if truthy?(on)
        __midi_each(ports, channels) do |p, c|
          __midi_send_pc("/note_on", p, c, n, vel)
          time_warp(sus - 0.01) { __midi_send_pc("/note_off", p, c, n, rel_vel) }
        end
        __midi_message "midi #{n}, #{vel}, #{where}"
      else
        __midi_message "midi #{n}, #{vel}, #{where}, on: 0"
      end
      nil
    end

    # ── Link: the shared timeline ─────────────────────────────────────────

    # Waits for the next quantum boundary (plus phase) on the shared
    # timeline, as Sonic Pi's link does against Ableton Link.
    def link(*args)
      params, opts = split_params_and_merge_opts_array(args)
      quantum = params[0] || opts.fetch(:quantum, 4)
      phase = params[1] || opts.fetch(:phase, 0)
      p = __p
      p.state_cache.clear
      p.last_sync = nil
      bpm = SonicPi.link_bpm
      anchored = !p.link_plain?          # a later run, or the tempo has changed: the session's timeline
      b = anchored ? p.link_beat_at_time(p.time) : (p.time - p.sched_ahead + DEFAULT_SCHED_AHEAD) * bpm / 60.0
      nb = (b / quantum).floor * quantum + phase
      nb += quantum while nb < b
      p.bpm = :link
      p.beat = nb.to_f
      target = anchored ? p.link_time_at_beat(nb) : (nb * 60.0 / bpm - p.sched_ahead + DEFAULT_SCHED_AHEAD).to_f
      if target > p.time
        p.time = target
        Fiber.yield([:sleep, p.time]) unless p.in_time_warp
      else
        p.time = target
      end
      nil
    end

    def link_sync(*args) = link(*args)

    def set_link_bpm!(bpm)
      raise ArgumentError, "use_bpm's BPM should be a positive value or :link. You tried to use: #{bpm}" unless bpm == :link || (bpm.is_a?(Numeric) && bpm > 0)
      raise ArgumentError, "set_link_bpm! requires a number for the bpm argument in the range 20 -> 999. You tried to use: #{bpm}" unless bpm.is_a?(Numeric) && bpm >= 20 && bpm <= 999
      p = __p
      @sched.set_link_bpm(bpm.to_f, p.start + p.time + p.sched_ahead)     # from the thread's moment, as it sounds
      @sched.rt_record(p, { kind: "studio", op: "link_bpm", value: bpm.to_f })
      nil
    end

    # ── Rings and lists ───────────────────────────────────────────────────

    def bools(*args) = args.map { |a| !(a == 0 || !a) }.ring

    def stretch(*args)
      raise ArgumentError, "stretch needs an even number of arguments, you passed: #{args.size} - #{args.inspect}" unless args.size.even?
      res = []
      args.each_slice(2) do |values, num_its|
        values = [values] unless values.respond_to?(:flat_map)
        res += knit(*values.to_a.flat_map { |v| [v, num_its] }).to_a
      end
      res.ring
    end

    def knit(*args)
      raise ArgumentError, "knit must have a even number of arguments, you passed: #{args.size} - #{args.inspect}" unless args.size.even?
      res = []
      args.each_slice(2) { |val, num_its| res += [val] * num_its if num_its > 0 }
      res.ring
    end

    def __redistribute(v1, v2)
      v_new = []
      while v1.length > 0 && v2.length > 0
        a1 = v1.shift
        a2 = v2.shift
        v_new.unshift(a1 + a2)
      end
      v1.length > 0 ? [v_new, v1] : [v_new, v2]
    end

    def spread(num_accents, size, *args)
      args_h = resolve_synth_opts_hash_or_array(args)
      beat_rotations = args_h[:rotate]
      return ([true] * size).ring if num_accents >= size
      return ([false] * size).ring if num_accents == 0
      v1 = [[true]] * num_accents
      v2 = [[false]] * (size - num_accents)
      v1, v2 = __redistribute(v1, v2)
      v1, v2 = __redistribute(v1, v2) while v2.length > 1
      res = (v1 + v2).flatten
      if beat_rotations && beat_rotations.is_a?(Numeric)
        beat_rotations = beat_rotations.abs
        while beat_rotations > 0
          res = res.rotate
          beat_rotations -= 1 if res.first == true
        end
      end
      res.ring
    end

    def range(start, finish, *args)
      start = start.to_f
      finish = finish.to_f
      if args.size == 1 && args.first.is_a?(Numeric)
        step_size = args.first
        inclusive = false
      else
        args_h = resolve_synth_opts_hash_or_array(args)
        step_size = (args_h[:step] || 1.0).to_f
        inclusive = args_h[:inclusive]
      end
      return [].ring if start == finish
      raise ArgumentError, "step size: opt for fn range should be a non-zero number" unless step_size != 0
      step_size = step_size.abs
      res = []
      cur = start
      if start < finish
        while inclusive ? cur.round(14) <= finish : cur.round(14) < finish
          res << cur.round(14)
          cur += step_size
        end
      else
        while inclusive ? cur.round(14) >= finish : cur.round(14) > finish
          res << cur.round(14)
          cur -= step_size
        end
      end
      res.ring
    end

    def line(start, finish, *args)
      start = start.to_f
      finish = finish.to_f
      return [].ring if start == finish
      args_h = resolve_synth_opts_hash_or_array(args)
      num_slices = args_h[:steps] || 4
      raise ArgumentError, "steps: opt for fn line should be a positive non-zero whole number" unless num_slices > 0
      if args_h[:inclusive]
        range(0, num_slices).scale((finish - start) / (num_slices - 1)) + start
      else
        range(0, num_slices).scale((finish - start) / num_slices) + start
      end
    end

    def halves(start, num_halves = 1)
      raise ArgumentError, "Start value for halves needs to be a number, got: #{start.inspect}" unless start.is_a?(Numeric)
      start = start.to_f
      return doubles(start, num_halves * -1) if num_halves < 0
      a = []
      val = start
      num_halves.times { a << val; val /= 2.0 }
      a.ring
    end

    def doubles(start, num_doubles = 1)
      raise ArgumentError, "Start value for doubles needs to be a number, got: #{start.inspect}" unless start.is_a?(Numeric)
      return halves(start, num_doubles * -1) if num_doubles < 0
      start = start.to_f
      a = []
      val = start
      num_doubles.times { a << val; val *= 2.0 }
      a.ring
    end

    def ramp(*args) = SonicPi::Ramp.new(args)
    def vector(*args) = SonicPi::Vector.new(args)

    def map(*args)
      if args.size > 1
        raise SonicPi::Lang::Core::MapArgError, "There needs to be an even number of args to map. Got: #{args.size}" unless args.size.even?
        return SPMap.new(*args)
      end
      SPMap.new(*args)
    end

    def inc(n) = n + 1
    def dec(n) = n - 1
    def factor?(val, factor) = factor == 0 ? false : (val % factor) == 0
    def on(condition, &blk) = (blk.call if truthy?(condition))

    def comment(*args, &block)
      raise ArgumentError, "comment requires a block." unless block
      nil
    end

    def uncomment(*args, &block)
      raise ArgumentError, "uncomment requires a block." unless block
      block.call
    end

    def ndefine(name, &block) = nil

    def spark_graph(*values)
      values = values.first if is_list_like?(values.first) && values.length == 1
      return "" if values.length == 0
      arr = values.to_a
      return "spark error: can't use nested arrays" if arr.flatten.length != arr.length
      ticks = ["▁", "▂", "▃", "▄", "▅", "▆", "▇"]
      nums = arr.map do |x|
        case x
        when TrueClass then 1
        when FalseClass then 0
        else x.respond_to?(:to_f) ? x.to_f : 0
        end
      end
      min = nums.min
      range = nums.max - nums.min
      scale = ticks.length - 1
      range = 1.0 if range.to_f == 0.0
      res = ""
      nums.each { |x| res += ticks[(((x - min) / range) * scale).round] }
      res
    end

    def spark(*values) = puts(spark_graph(*values))

    # ── Theory ────────────────────────────────────────────────────────────

    def rest?(n)
      case n
      when Numeric then false
      when Symbol then n == :r || n == :rest
      when NilClass then true
      when Hash, SPMap then n.key?(:note) ? (n[:note].nil? || n[:note] == :r || n[:note] == :rest) : false
      else false
      end
    end

    def octs(start, num_octs = 1)
      a = []
      num_octs.times { |i| a << (note(start) + (12 * i)) }
      a.ring
    end

    def midi_notes(*args) = args.map { |a| note(a) }.ring

    def chord_invert(notes, shift)
      raise "Inversion shift value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
      shift = shift.round
      raise "Notes must be a list of notes, got #{notes.inspect}" unless is_list_like?(notes)
      a = notes.to_a
      if shift > 0
        chord_invert(a[1..] + [a[0] + 12], shift - 1)
      elsif shift < 0
        chord_invert((a[0, a.size - 1] + [a[-1] - 12]).sort, shift + 1)
      else
        a.ring.sort
      end
    end

    ROMAN = { "I" => 1, "V" => 5, "X" => 10, "L" => 50, "C" => 100, "D" => 500, "M" => 1000 }

    def __to_roman(n)
      out = ""
      [[1000, "M"], [900, "CM"], [500, "D"], [400, "CD"], [100, "C"], [90, "XC"], [50, "L"], [40, "XL"], [10, "X"], [9, "IX"], [5, "V"], [4, "IV"], [1, "I"]].each do |v, s|
        while n >= v
          out += s
          n -= v
        end
      end
      out
    end

    def __from_roman(numeral)
      s = numeral.to_s.upcase
      return nil unless s.each_char.all? { |c| ROMAN.key?(c) }
      vals = s.each_char.map { |c| ROMAN[c] }
      result = 0
      i = 0
      while i < vals.length
        if i < vals.length - 1 && vals[i + 1] > vals[i]
          result += vals[i + 1] - vals[i]
          i += 2
        else
          result += vals[i]
          i += 1
        end
      end
      return nil if result > 4999 || __to_roman(result) != s
      result
    end

    def __degree_index(degree)
      num = if degree.is_a?(Numeric)
              degree
            else
              s = degree.to_s
              digits = s.start_with?("-") ? s[1..] : s
              !digits.empty? && digits.each_char.all? { |c| c >= "0" && c <= "9" } ? s.to_i : __from_roman(s)
            end
      if num.nil? || num <= 0
        raise SonicPi::Scale::InvalidDegreeError, "Invalid scale degree #{degree.inspect}, scale degree must be a valid number or roman numeral greater than 0"
      end
      num - 1
    end

    def degree(degree, tonic, scale_name)
      notes = Theory.scale(tonic, scale_name, 1)
      augmentation = 0
      unless degree.is_a?(Numeric)
        degree = degree.to_s.downcase
        if degree.start_with?("p")
          degree = degree[1..]
        elsif degree.start_with?("aa")
          augmentation = 2
          degree = degree[2..]
        elsif degree.start_with?("a")
          augmentation = 1
          degree = degree[1..]
        elsif degree.start_with?("dd")
          augmentation = -2
          degree = degree[2..]
        elsif degree.start_with?("d")
          augmentation = -1
          degree = degree[1..]
        end
      end
      octave, index = __degree_index(degree).divmod(notes.length - 1)
      span = notes.last - notes.first
      notes[index] + octave * span + augmentation
    end

    def chord_degree(degree, tonic, scale = :major, number_of_notes = 4, *opts)
      opts = { invert: 0 }.merge(resolve_synth_opts_hash_or_array(opts))
      degree_int = __degree_index(degree)
      notes = Theory.scale(tonic, scale, 2)
      min_notes = degree_int + (number_of_notes * 2) - 1
      if notes.length < min_notes
        per_octave = (notes.length - 1) / 2
        notes = Theory.scale(tonic, scale, (min_notes.to_f / per_octave).ceil)
      end
      picked = []
      notes.drop(degree_int).each_with_index { |n, i| picked << n if i.even? }
      chord_invert(picked.take(number_of_notes), opts[:invert]).ring
    end

    def __note_name(n) = NoteInfo::NAMES[Note.midi(n).to_i % 12]

    def note_range(start_note, end_note, *opts)
      opts_h = resolve_synth_opts_hash_or_array(opts)
      start_note = note(start_note)
      end_note = note(end_note)
      low, high = [start_note, end_note].minmax
      pool = (low..high).to_a
      if opts_h[:pitches]
        classes = opts_h[:pitches].map { |x| __note_name(x) }
        pool = pool.select { |n| classes.include?(__note_name(n)) }
      end
      start_note == low ? pool.ring : pool.ring.reverse
    end

    def note_info(n, args = nil)
      raise Exception, "note_info argument must be a valid note. Got nil." if n.nil?
      return NoteInfo.new(n) if args.nil?
      NoteInfo.new(n, resolve_synth_opts_hash_or_array(args)[:octave])
    end

    def scale_names = SonicPi::Data::SCALES.keys.sort_by(&:to_s).ring
    def chord_names = SonicPi::Data::CHORD_NAMES.ring
    def synth_names = SonicPi::Data::SYNTH_NAMES.ring
    def fx_names = SonicPi::Data::FX_NAMES.ring

    def use_tuning(tuning, fundamental_note = :c, &block)
      raise "use_tuning does not work with a do/end block. Perhaps you meant with_tuning" if block
      raise "tuning value must be a symbol like :just or :equal, got #{tuning.inspect}" unless tuning.is_a?(Symbol)
      __set_local(:tuning, [tuning, fundamental_note])
    end

    def with_tuning(tuning, fundamental_note = :c, &block)
      raise "with_tuning requires a do/end block. Perhaps you meant use_tuning" unless block
      raise "tuning value must be a symbol like :just or :equal, got #{tuning.inspect}" unless tuning.is_a?(Symbol)
      __with_local(:tuning, [tuning, fundamental_note]) { block.call }
    end

    def set_cent_tuning!(shift)
      @sched.cent_tuning = shift
    end

    # ── The current state ─────────────────────────────────────────────────

    def current_transpose = __local(:transpose) || 0
    def current_octave = __local(:octave_shift) || 0
    def current_cent_tuning = __local(:cent_tuning) || 0
    def current_debug = !__local(:synth_silent)
    def current_arg_checks = __local(:check_synth_args)
    def current_random_seed = __rand.idx + __rand.seed
    def current_random_source = __rand.source
    def current_beat_duration = __p.sleep_mul
    def beat = __p.beat
    def rt(t) = t / __p.sleep_mul
    def bt(t) = t * __p.sleep_mul
    def vt = __p.time.round(6)
    def current_volume = @sched.volume
    def current_drive = Math.log(@sched.drive / 0.25) / Math.log(16)

    def block_duration(&block)
      t1 = __p.time
      block.call
      __p.time.to_r - t1.to_r
    end

    def block_slept?(&block) = block_duration(&block) > 0

    def with_real_time(&blk)
      raise ArgumentError, "with_real_time must be called with a do/end block. Perhaps you meant use_real_time" unless blk
      p = __p
      sat = p.own_sched_ahead   # its own, or nil: after the block it follows the session's again
      p.sched_ahead = 0.0
      res = __with_local(:real_time, true) { blk.call }
      p.sched_ahead = sat
      res
    end

    def with_sched_ahead_time(t, &blk)
      raise ArgumentError, "with_sched_ahead_time must be called with a do/end block. Perhaps you meant use_sched_ahead_time" unless blk
      p = __p
      sat = p.own_sched_ahead
      p.sched_ahead = t.to_f
      res = blk.call
      p.sched_ahead = sat
      res
    end

    def wait(time) = time.is_a?(Symbol) ? sync(time) : sleep(time)

    def with_swing(*args, &blk)
      raise ArgumentError, "with_swing must be called with a do/end block." unless blk
      params, opts = split_params_and_merge_opts_array(args)
      shift = params[0] || opts.fetch(:shift, 0.1)
      pulse = params[1] || opts.fetch(:pulse, 4)
      key = (params[2] || opts.fetch(:tick, :swing)).to_sym
      offset = params[3] || opts.fetch(:offset, 0)
      raise ArgumentError, "with_swing shift should be a number. Got: #{shift.inspect}" unless shift.is_a?(Numeric)
      raise ArgumentError, "with_swing pulse should be a positive number. Got: #{pulse.inspect}" unless pulse.is_a?(Numeric) && pulse > 0
      raise ArgumentError, "with_swing offset should be a number. Got: #{offset.inspect}" unless offset.is_a?(Numeric)
      offset = offset.round
      if ((tick(key) + offset) % pulse) == 0
        time_warp(shift) { blk.call }
      else
        blk.call
      end
      nil
    end

    def tuplets(tuplet_list, opts = {}, &blk)
      duration = opts.fetch(:duration, 1)
      swing_s = bt(opts.fetch(:swing, 0))
      swing_pulse = opts.fetch(:swing_pulse, 2)
      swing_offset = opts.fetch(:swing_offset, 0) + 1
      tuplet_list.each do |el|
        if is_list_like?(el)
          n = el.size
          __with_density(n) do
            el.each_with_index do |tuplet, idx|
              current_swing = ((n % swing_pulse) == 0 && ((idx + swing_offset) % swing_pulse) == 0) ? swing_s : 0
              time_warp(rt(current_swing)) { blk.call(tuplet) }
              sleep duration
            end
          end
        else
          time_warp { blk.call(el) }
          sleep duration
        end
      end
    end

    # ── the rest of native's language: names it keeps, and what its old ones say ──

    def invert_chord(*args) = chord_invert(*args)
    def live_state(*args) = get(*args)
    # the engine's output level, as it last reported it (native's studio.amp: [time, amp], and [0.0, 1.0] before)
    def current_amp = @sched.amp
    # with_fx in a thread of its own, so the block's time is not the caller's
    def with_afx(fx_name, *args, &block) = in_thread { with_fx(fx_name, *args, &block) }

    def use_timing_warnings(v, &block)
      raise "use_timing_warnings does not work with a do/end block. Perhaps you meant with_timing_warnings" if block
      __set_local(:disable_timing_warnings, !v)
    end

    def with_timing_warnings(v, &block)
      raise "with_timing_warnings requires a do/end block. Perhaps you meant use_timing_warnings" unless block
      __with_local(:disable_timing_warnings, !v) { block.call }
    end

    # the MIDI ports a program's MIDI goes to unless it says port:, as native narrows them: '*' is every port; a
    # string, symbol, regexp, number or proc filters the ports there are (midi_available_ports)
    def midi_available_ports = ["*".freeze].freeze
    def use_midi_ports(*filters_and_procs)
      if filters_and_procs.size == 1 && filters_and_procs[0] == "*"
        __set_local(:midi_ports, "*".freeze)
        return "*"
      end
      strings, others = filters_and_procs.partition { |fp| fp.is_a?(String) }
      others.unshift(Regexp.new(".*" + strings.map { |sf| Regexp.escape(sf) }.join(".*") + ".*")) unless strings.empty?
      candidates = midi_available_ports.to_a.dup
      others.each do |f|
        case f
        when Symbol then candidates.keep_if { |c| c == f.to_s }
        when Regexp then candidates.keep_if { |c| f.match(c) }
        when Integer then candidates = [candidates[f % candidates.size]] unless candidates.empty?
        when NilClass then nil
        when Proc
          raise "MIDI Port Filter Proc accepts 1 argument only. Found #{f.arity}" unless f.arity == 1
          candidates = f.call(candidates)
          candidates = [candidates] unless is_list_like?(candidates)
        else raise "Unknown MIDI port filter type: #{f.class} - got: #{f.inspect}"
        end
      end
      __set_local(:midi_ports, candidates.freeze)
    end

    # what native's retired names say
    def with_tempo(*args, &block) = raise(SonicPi::Lang::Core::DeprecationError, "The function with_tempo is deprecated since v2.0. Please consider use_bpm or with_bpm.")
    def use_fx(*args, &block) = raise("use_fx isn't supported in this version of Sonic Pi. Perhaps you meant with_fx")
    def pitch_ratio(*args) = raise("The fn pitch_ratio has been renamed. Please use the new name: pitch_to_ratio")
    def use_sample_pack(pack, &block) = raise("Sorry, use_sample_pack is no longer supported since v2.11. \n  Please read Section 3.7 of the tutorial for a more powerful replacement.")
    def with_sample_pack(pack, &block) = raise("Sorry, with_sample_pack is no longer supported since v2.11. \n  Please read Section 3.7 of the tutorial for a more powerful replacement.")
    def current_sample_pack_aliases(*args) = raise("Sorry, current_sample_pack_aliases is no longer supported since v2.10. Please read Section 3.7 of the tutorial for a more powerful replacement.")
    def with_sample_pack_as(*args) = raise("Sorry, with_sample_pack_as is no longer supported since v2.10. Please read Section 3.7 of the tutorial for a more powerful replacement.")
    def use_sample_pack_as(*args) = raise("Sorry, use_sample_pack_as is no longer supported since v2.10. Please read Section 3.7 of the tutorial for a more powerful replacement.")

    def use_timing_guarantees(v, &block)
      raise "use_timing_guarantees does not work with a do/end block. Perhaps you meant with_timing_guarantees" if block
      __set_local(:timing_guarantees, v)
    end

    def with_timing_guarantees(v, &block)
      raise "with_timing_guarantees requires a do/end block. Perhaps you meant use_timing_guarantees" unless block
      __with_local(:timing_guarantees, v) { block.call }
    end

    # reset: the settings, density and random stream the thread began with;
    # clear: the defaults, seed 0 and no random source. Both drop the ticks and
    # the last node (Sonic Pi's thread-local locals), and neither touches what
    # Sonic Pi keeps as system locals: the tempo, and real time mode.
    def reset
      p = __p
      return clear unless p.reset_to
      locals, density, rand = p.reset_to
      __restart_locals(p, locals.dup, density)
      r = p.rand
      r.seed = rand.seed
      r.idx = rand.idx
      r.source = rand.source
      r.new_thread_idx = rand.new_thread_idx
      nil
    end

    def clear
      p = __p
      __restart_locals(p, @sched.default_locals, 1.0)
      r = p.rand
      r.seed = 0
      r.idx = 0
      r.source = nil
      r.new_thread_idx = 0
      nil
    end

    def __restart_locals(p, locals, density)
      real_time = p.locals[:real_time]
      locals.delete(:real_time)
      locals.delete(:last_node)
      locals[:real_time] = real_time unless real_time.nil?
      p.locals = locals
      p.density = density
      p.counters.clear
    end

    # ── Asserts ───────────────────────────────────────────────────────────

    def assert(arg, msg = nil)
      raise SonicPi::Lang::Core::AssertionError, "Assert failed! #{msg}" unless arg
      arg
    end

    def assert_not(arg, msg = nil)
      raise SonicPi::Lang::Core::AssertionError, "Assert not failed! #{msg}" if arg
      arg
    end

    def assert_equal(arg1, arg2, msg = nil)
      raise SonicPi::Lang::Core::AssertionError, "Assert failed! #{SonicPi.log_inspect(arg1)} is not equal to #{SonicPi.log_inspect(arg2)}. #{msg}" unless arg1 == arg2
      arg1
    end

    def assert_not_equal(arg1, arg2, msg = nil)
      raise SonicPi::Lang::Core::AssertionError, "Assert note equal failed! #{SonicPi.log_inspect(arg1)} is equal to #{SonicPi.log_inspect(arg2)}. #{msg}" if arg1 == arg2
      arg1
    end

    def assert_similar(a, b, msg = nil)
      return assert_equal(a.to_f.round(5), b.to_f.round(5), msg) if a.is_a?(Numeric)
      assert_equal(a, b, msg)
    end

    def assert_error(klass = Exception, &blk)
      raised = nil
      begin
        blk.call
      rescue Exception => e
        return nil if e.is_a?(klass)
        raised = e.class
      end
      raise SonicPi::Lang::Core::AssertionError, "Assert error failed! #{klass.inspect} not raised by running do/end block, instead block raised #{raised.inspect}" if raised
      raise SonicPi::Lang::Core::AssertionError, "Assert error failed! No errors raised by running do/end block"
    end

    # ── The studio ────────────────────────────────────────────────────────

    def set_volume!(vol, now = false, silent = false)
      vol = 0.0 if vol < 0
      vol = 5.0 if vol > 5
      @sched.volume = vol.to_f
      @sched.rt_record(__p, { kind: "studio", op: "volume", value: vol.to_f })
      nil
    end

    def set_drive!(amount, now = false, silent = false)
      amount = 0 if amount < 0
      amount = 1 if amount > 1
      @sched.drive = 0.25 * (16 ** amount)
      @sched.rt_record(__p, { kind: "studio", op: "drive", value: @sched.drive })
      nil
    end

    def set_mixer_control!(opts)
      @sched.rt_record(__p, { kind: "studio", op: "mixer", value: __recorded_args(resolve_synth_opts_hash_or_array(opts)) })
      nil
    end

    def reset_mixer!
      @sched.rt_record(__p, { kind: "studio", op: "mixer_reset" })
      nil
    end

    def set_control_delta!(t = 0.005) = nil
    def set_audio_latency!(delta_ms) = nil
    def set_recording_bit_depth!(d) = nil

    def status = SPMap.new(ugens: 0, synths: 0, groups: 0, sdefs: 0, avg_cpu: 0.0, peak_cpu: 0.0, nom_samp_rate: 48000.0, act_samp_rate: 48000.0, audio_busses: 0, control_busses: 0, audio_ins: 0, audio_outs: 2)
    def scsynth_info = SPMap.new(sample_rate: 48000.0, sample_dur: 1.0 / 48000, radians_per_sample: 2 * Math::PI / 48000, control_rate: 48000.0 / 64, control_dur: 64.0 / 48000, subsample_offset: 0.0, num_output_busses: 2, num_input_busses: 2, num_audio_busses: 1024, num_control_busses: 4096, num_buffers: 4096)

    def version = SonicPi::WebVersion.new

    def sample_free(*paths)
      paths.each do |path|
        path = [path] unless is_list_like?(path)
        filts, _ = __sample_split(path.to_a)
        resolve_sample_paths(filts).each do |p|
          next unless Samples.loaded?(p)
          Samples.unload(p)
          @sched.sample_freed(__p, Samples.basename(p))
        end
      end
      nil
    end

    def sample_free_all
      Samples.loaded_paths.each { |p| @sched.sample_freed(__p, Samples.basename(p)) }
      Samples.unload_all
      nil
    end

    # Replaces the running buffer with an example, as the GUI does it.
    def load_example(example_name)
      @sched.rt_record(__p, { kind: "load_example", name: example_name.to_s })
      nil
    end

    # Runs code as a job of its own, starting now.
    def run_code(code) = @sched.run_code(self, code.to_s, __p)
  end

  class WebVersion
    def major = 5
    def minor = 0
    def patch = 0
    def to_s = "v5.0.0-web"
    def inspect = to_s
  end
end
