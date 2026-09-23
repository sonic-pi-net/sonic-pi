# SPDX-License-Identifier: AGPL-3.0-or-later
# The language: the verbs a program calls, over the scheduler's current
# thread. One object serves every thread, as Sonic Pi's runtime does; what
# differs per thread lives on the Process.
module SonicPi
  # what play returns when nothing sounds (a rest, or should_trigger? said no): native's BlankNode, which answers
  # every node's question as a node already gone
  class BlankNode
    attr_reader :args
    def initialize(args) = (@args = args)
    def name = nil
    def to_s = "#<SonicPi::BlankNode>"
    def inspect = to_s
    def kill(now = false) = self
    def pause(now = false) = self
    def run(now = false) = self
    def ctl(*args) = self
    def ctl_now(*args) = self
    def control(*args) = self
    def live? = false
    def destroyed? = true
    def paused? = false
    def running? = false
    def state = :destroyed
    def blank_node? = true
  end

  class SynthNode
    attr_reader :name, :args, :info, :id, :ref
    # when the synth ends (logical time), or for an fx, its with_fx frame
    attr_accessor :ends_at, :frame
    attr_accessor :frame_place   # live_audio's: the fx it sounds into (Scheduler#live_place)
    def initialize(name, args, info = nil, id = nil, ref = nil)
      @name = name
      @args = args
      @info = info
      @id = id
      @ref = ref
    end
    def to_s = "#<SonicPi::Node @name=#{@name}>"

    # native's Node: what a program can do with the sound play gave it — as the language's own kill and control
    # (so they are recorded and logged as those are), pause and run, and what state it is in
    def kill(now = false) = (SonicPi.current_lang.kill(self, false); self)
    def control(*args) = (SonicPi.current_lang.__control([self, *args], false); self)
    def ctl(*args) = control(*args)
    def ctl_now(*args) = control(*args)
    def pause(now = false) = (SonicPi.current_lang.__node_run(self, false); @paused = true; self)
    def run(now = false) = (SonicPi.current_lang.__node_run(self, true); @paused = false; self)
    def destroyed? = @killed || SonicPi.current_lang.__node_ended?(self)
    def live? = !destroyed?
    def paused? = !destroyed? && !!@paused
    def running? = !destroyed? && !@paused
    def state = destroyed? ? :destroyed : @paused ? :paused : :running
    def killed! = (@killed = true)
    def to_i = @id.to_i
    def to_f = @id.to_f
    def blank_node? = false
  end

  class Language
    include SonicPi::RandVerbs
    def initialize(sched)
      @sched = sched
    end

    # A bare name that is nothing is a NameError in Ruby and a NoMethodError
    # in mruby; the trace says one thing for both.
    def method_missing(name, *args, &blk)
      if args.empty? && !blk
        raise NameError, "undefined local variable or method '#{name}'"
      end
      raise NoMethodError, "undefined method '#{name}'"
    end

    def respond_to_missing?(name, include_private = false) = false

    # ── The thread at hand ────────────────────────────────────────────────

    def __p = @sched.current
    def __rand = __p.rand
    def __local(k) = __p.locals[k]
    def __set_local(k, v) = (__p.locals[k] = v)
    def __with_local(k, v)
      old = __local(k)
      __set_local(k, v)
      res = yield
      __set_local(k, old)
      res
    end

    # ── Output ────────────────────────────────────────────────────────────

    def puts(*msgs)
      @sched.output_line(__p, msgs.map { |m| SonicPi.log_inspect(m) }.join(" "))
      nil
    end
    alias_method :print, :puts

    def __log(text) = @sched.log_line(__p, text)

    # ── Time ──────────────────────────────────────────────────────────────

    def sleep(beats)
      p = __p
      p.last_sync = nil
      p.slept = true if beats != 0
      p.state_cache.clear unless p.state_cache.empty?
      from = p.time
      p.advance!(beats)
      # Sonic Pi's sleep wakes 0.2s early, and not at all under 0.2s (Process#wall)
      p.wall = p.start + p.time - 0.2 if !p.in_time_warp && p.start + p.time - p.wall - 0.2 >= 0.200001
      if beats > 0 && !p.in_time_warp
        @sched.rt_sleep(p, from, beats)
        Fiber.yield([:sleep, p.time])
      end
      raise SonicPi::Stop if @sched.stop_after && p.time > @sched.stop_after
      nil
    end

    def current_beat = __p.beat
    def current_bpm = __p.bpm_value
    def current_time = __p.time
    def current_bpm_mode = (__p.bpm == :link ? :link : __p.bpm_value)

    def __resolve_bpm(bpm)
      return :link if bpm == :link
      raise ArgumentError, "use_bpm's bpm must be a number or :link, got: #{bpm.inspect}" unless bpm.is_a?(Numeric)
      bpm
    end

    def __change_bpm!(bpm)
      p = __p
      if bpm == :link
        unless p.bpm == :link
          p.bpm = :link
          # re-anchored on the timeline: the beat Link has at the thread's moment
          # (at 60 bpm, before any tempo change, the time itself)
          p.beat = !p.link_plain? || SonicPi.link_bpm != 60.0 ? p.link_beat_at_time(p.time) : p.time.to_f
        end
      else
        p.bpm = bpm
      end
    end

    def use_bpm(bpm, *args, &block)
      raise ArgumentError, "use_bpm does not work with a block. Perhaps you meant with_bpm" if block
      __change_bpm!(__resolve_bpm(bpm))
    end

    def with_bpm(bpm, *args, &block)
      raise ArgumentError, "with_bpm must be called with a do/end block. Perhaps you meant use_bpm" unless block
      bpm = __resolve_bpm(bpm)
      current = current_bpm_mode
      use_bpm bpm
      res = block.call
      use_bpm current
      res
    end

    def use_bpm_mul(mul, &block)
      raise ArgumentError, "use_bpm_mul must not be called with a block. Perhaps you meant with_bpm_mul" if block
      raise ArgumentError, "use_bpm_mul's mul should be a positive value. You tried to use: #{mul}" unless mul > 0
      __p.density = (__p.density * mul).to_f
    end

    def with_bpm_mul(mul, &block)
      raise ArgumentError, "with_bpm_mul must be called with a do/end block. Perhaps you meant use_bpm_mul" unless block
      raise ArgumentError, "with_bpm_mul's mul should be a positive value. You tried to use: #{mul}" unless mul > 0
      __with_density(mul) { block.call }
    end

    def __with_density(d)
      prev = __p.density
      __p.density = (prev * d).to_f
      res = yield
      __p.density = prev
      res
    end

    def density(d, &block)
      raise ArgumentError, "density must be called with a do/end block." unless block
      raise ArgumentError, "density must be a positive number. Got: #{d.inspect}." unless d.is_a?(Numeric) && d > 0
      reps = d < 1 ? 1.0 : d
      __with_density(d) do
        if block.arity == 0
          reps.times { block.call }
        else
          reps.times { |idx| block.call(idx) }
        end
      end
    end

    def __with_preserved_time
      p = __p
      time, beat = p.time, p.beat
      yield
      p.time, p.beat = time, beat
    end

    def time_warp(times = 0, params = nil, &block)
      raise ArgumentError, "time_warp requires a do/end block" unless block
      p = __p
      prev_slept, prev_synced = p.slept, p.synced
      had_params = params
      times = [times] if times.is_a?(Numeric)
      params ||= times
      params_size = params.size
      already = p.in_time_warp
      p.time_warp_start = p.time unless already   # where the outermost warp began: get checks it (native's)
      p.in_time_warp = true
      times.each_with_index do |delta, idx|
        __with_preserved_time do
          sleep delta
          case block.arity
          when 0 then block.call
          when 1 then block.call(params[idx % params_size])
          when 2 then had_params ? block.call(delta, params[idx % params_size]) : block.call(delta, idx)
          when 3 then block.call(delta, params[idx % params_size], idx)
          else raise ArgumentError, "block for time_warp should only accept 0, 1, 2 or 3 parameters. You gave: #{block.arity}."
          end
        end
      end
      p.in_time_warp = already
      p.synced, p.slept = prev_synced, prev_slept
    end

    def at(times = 0, params = nil, &block)
      raise ArgumentError, "at must be called with a do/end block" unless block
      had_params = params
      times = [times] if times.is_a?(Numeric)
      params ||= times
      params_size = params.size
      times.each_with_index do |t, idx|
        in_thread do
          sleep t
          case block.arity
          when 0 then block.call
          when 1 then block.call(params[idx % params_size])
          when 2 then had_params ? block.call(t, params[idx % params_size]) : block.call(t, idx)
          when 3 then block.call(t, params[idx % params_size], idx)
          else raise ArgumentError, "block for at should only accept 0, 1, 2 or 3 parameters. You gave: #{block.arity}."
          end
        end
      end
    end

    # ── Threads ───────────────────────────────────────────────────────────

    def in_thread(*opts, &block)
      args_h = resolve_synth_opts_hash_or_array(opts)
      delay = args_h[:delay]
      raise ArgumentError, "in_thread's delay: opt must be a number, got #{delay.inspect}" if delay && !delay.is_a?(Numeric)
      @sched.spawn(self, args_h, &block)
    end

    def loop(&block)
      raise ArgumentError, "loop needs a block" unless block
      p = __p
      while true
        p.synced = false
        p.slept = false
        block.call
        raise SonicPi::Lang::Core::ZeroTimeLoopError, "loop did not sleep or sync!" unless p.slept || p.synced
      end
    end

    def stop = raise(SonicPi::Stop)

    # User functions live on this one object, so every thread sees them.
    # A name that is already a verb of the language is refused, as Sonic Pi
    # refuses it; a name a program defined before is simply replaced.
    def define(name, &block)
      raise ArgumentError, "define must be called with a do/end block" unless block
      name = name.to_sym
      already = __user_methods.include?(name)
      if !already && respond_to?(name)
        raise ArgumentError, "A function called #{name} is already part of Sonic Pi's core API. Please choose another name."
      end
      if name.to_s[0] >= "A" && name.to_s[0] <= "Z"
        __log("Warning - defined function '#{name}' starts with a capital letter and may not behave as expected. Please start functions with a lower-case letter.")
      end
      __user_methods << name unless already
      singleton_class.send(:define_method, name, &block)
    end

    def __user_methods = (@__user_methods ||= [])

    # defonce: the block runs once, in a thread of its own; until it has, the
    # function answers :undefined, and afterwards its value.
    def defonce(name, *opts, &block)
      raise ArgumentError, "defonce must be called with a do/end block" unless block
      args_h = resolve_synth_opts_hash_or_array(opts)
      name = name.to_sym
      if args_h[:override] || !__user_methods.include?(name)
        define(name) { :undefined }
        in_thread do
          val = block.call
          define(name) { val }
        end
      end
    end

    def live_loop(name = nil, *args, &block)
      raise ArgumentError, "live_loop needs to have a unique name. For example: live_loop :foo" unless name
      raise ArgumentError, "live_loop's name needs to be a string or symbol, got: #{name.inspect}. Example usage: live_loop :foo" unless name.is_a?(Symbol) || name.is_a?(String)
      ll_name = "live_loop_#{name}".to_sym
      raise ArgumentError, "live_loop #{name.inspect} must be called with a do/end block" unless block
      args_h = resolve_synth_opts_hash_or_array(args)
      sync_sym = args_h[:sync]
      sync_bpm_sym = args_h[:sync_bpm]
      sync_sym = nil if sync_bpm_sym
      raise SonicPi::Lang::Core::LiveLockError, "livelock detection - live_loop cannot sync with itself - please choose another sync name for live_loop #{name.inspect}" if name == sync_sym || name == sync_bpm_sym
      delay = args_h[:delay]
      raise ArgumentError, "live_loop's delay: opt must be a number, got #{delay.inspect}" if delay && !delay.is_a?(Numeric)
      auto_cue = args_h.key?(:auto_cue) ? args_h[:auto_cue] : true
      case block.arity
      when 0 then define(ll_name) { |a| block.call }
      when 1 then define(ll_name) { |a| block.call(a) }
      else raise ArgumentError, "Live loop block must only accept 0 or 1 args"
      end
      existing = @sched.named_process(ll_name, __p.wall)
      if existing
        existing.redefs += 1                    # a Run that redefines a running loop: same thread, new code
        @sched.move_loop(existing, __p)         # and into the with_fx it is run from
      end
      scope_num = @sched.scope_slot(ll_name)
      in_thread(name: ll_name, delay: delay, sync: sync_sym, sync_bpm: sync_bpm_sym, defer: true) do
        res = args_h.key?(:init) ? args_h[:init] : 0
        use_random_seed args_h[:seed] if args_h[:seed]
        run_loop = lambda do
          loop do
            __live_loop_cue(name) if auto_cue
            res = send(ll_name, res)
          end
        end
        begin
          if scope_num
            with_fx(:scope_out, scope_num: scope_num) { run_loop.call }   # every live_loop feeds a scope
          else
            run_loop.call
          end
        ensure
          @sched.release_scope_slot(ll_name)
        end
      end
    end

    # ── Cues ──────────────────────────────────────────────────────────────

    def __cue_segment(s) = s.to_s.each_char.map { |c| " #*,?/[]{}".include?(c) ? "_" : c }.join

    def __cue_address(k, prefix = "cue")
      s = k.to_s
      s = s.start_with?("/") ? s : "/#{prefix}/#{s}"
      s.each_char.map { |c| " #*,?[]{}".include?(c) ? "_" : c }.join
    end

    def __sync_addresses(k)
      return %w[cue set live_loop].map { |pre| "/#{pre}/#{__cue_segment(k)}" } if k.is_a?(Symbol)
      s = k.to_s
      [s.start_with?("/") ? s : "/cue/#{s}"]
    end

    # cue's args, as native checks them: a map's keys symbols and its values already immutable; a list's each made
    # so (native's cue, lang/core.rb), or it says which one it could not take
    IMMUTABLE_TYPES = "Must be immutable -  currently accepted types: numbers, symbols, booleans, nil and frozen strings, or vectors/rings/frozen arrays/maps of immutable values"
    def cue(k, *opts)
      val = if opts.size == 1 && opts[0].is_a?(Hash)
              opts[0].each do |key, v|
                raise ArgumentError, "Invalid cue key type. Must be a Symbol" unless key.is_a?(Symbol)
                raise ArgumentError, "Invalid cue argument #{v.inspect} with key #{key.inspect} due to unrecognised type: (#{v.class}). #{IMMUTABLE_TYPES}" unless TimeState.safe?(v)
              end
              opts[0]
            else
              opts.each_with_index do |v, idx|
                v = TimeState.make_safe(v)
                raise ArgumentError, "Invalid cue argument #{v.inspect} in position #{idx} due to unrecognised type: (#{v.class}). #{IMMUTABLE_TYPES}" unless TimeState.safe?(v)
              end
              opts
            end
      __log("cue #{k.inspect}, #{SonicPi.log_inspect(val)}") unless __local(:suppress_cue_logging)
      address = __cue_address(k)
      ev = @sched.cue(__p, __p.priority, address, val)
      __p.state_cache.unshift([address, ev])
      nil
    end

    def __live_loop_cue(name) = @sched.cue(__p, -100, "/live_loop/#{name}", [])

    def sync(*args)
      __sync_event(*args).val
    end

    def sync_bpm(*args)
      params, opts = split_params_and_merge_opts_array(args)
      opts[:bpm_sync] = true
      sync(*params, opts)
    end

    def __sync_event(*args)
      p = __p
      raise SonicPi::Lang::Core::TimeTravelError, "Calling sync within a time_warp is not supported. " if p.in_time_warp
      params, opts = split_params_and_merge_opts_array(args)
      k = params[0]
      bpm_sync = truthy?(opts[:bpm_sync])
      addresses = __sync_addresses(k)
      cue_id = k.is_a?(Symbol) ? "/{cue,set,live_loop}/#{__cue_segment(k)}" : addresses[0]
      __log("sync #{k.inspect}") unless __local(:suppress_cue_logging)
      p.state_cache.clear unless p.state_cache.empty?
      @sched.rt_sync(p, addresses)
      key = p.last_sync ? p.last_sync.key : p.key
      ev = @sched.wait_for_cue(key, addresses)
      p.last_sync = ev
      p.synced = true
      p.time = ev.time - p.start       # the cue's moment, on this job's own clock
      p.beat = ev.beat
      if bpm_sync
        __change_bpm!(ev.bpm)
        __log("synced #{cue_id.inspect}. Inheriting bpm of #{SonicPi.log_inspect(current_bpm)} ") unless __local(:suppress_cue_logging)
      else
        __log("synced #{cue_id.inspect} ") unless __local(:suppress_cue_logging)
      end
      ev
    end

    # ── Tick ──────────────────────────────────────────────────────────────

    DEFAULT_TICK = :___sonic_pi_default_tick_key___

    def __tick_args(k, args)
      k = k.to_sym if k.is_a?(String)
      if k.is_a?(Symbol)
        opts = args.first || {}
      else
        opts = k
        k = DEFAULT_TICK
      end
      raise "Tick key must be a symbol, got #{k.class}: #{k.inspect}" unless k.is_a?(Symbol)
      raise "Tick opts must be key value pairs, got: #{opts.inspect}" unless opts.is_a?(Hash)
      [k, opts]
    end

    def tick(k = DEFAULT_TICK, *args)
      k, opts = __tick_args(k, args)
      step = opts[:step] || 1
      offset = opts[:offset] || 0
      counters = __p.counters
      if counters[k]
        next_val = counters[k][1]
        counters[k] = [next_val + step - 1, next_val + step]
        next_val + step - 1 + offset
      else
        counters[k] = [step - 1, step]
        step - 1 + offset
      end
    end

    def look(k = DEFAULT_TICK, *args)
      return args[1] if args[1].is_a?(Numeric) && args.size == 1
      k, opts = __tick_args(k, args)
      offset = opts[:offset] || 0
      c = __p.counters[k]
      (c ? c[0] : 0) + offset
    end

    def tick_set(k = DEFAULT_TICK, v = nil)
      if k.is_a?(Numeric)
        v = k
        k = DEFAULT_TICK
      end
      raise "Tick key must be a symbol, got #{k.class}: #{k.inspect}" unless k.is_a?(Symbol)
      raise "Tick value must be a number, got #{v.class}: #{v.inspect}" unless v.is_a?(Numeric)
      __p.counters[k] = [v, v]
      v
    end

    def tick_reset(k = DEFAULT_TICK)
      k = k.to_sym if k.is_a?(String)
      __p.counters.delete(k)
      nil
    end

    def tick_reset_all
      __p.counters.clear
      nil
    end

    # ── Notes, chords, scales ─────────────────────────────────────────────

    def note(n, *args)
      case n
      when Numeric then return n                # as Sonic Pi: a number is itself, whatever the opts
      when Symbol then return nil if n == :r || n == :rest
      when NilClass then return nil
      when Proc then return note(n.call, *args)
      when Hash then raise "Unable to create a note from the Map: #{n.inspect}"
      end
      return Note.midi(n) if args.empty?
      octave = resolve_synth_opts_hash_or_array(args)[:octave]
      return Note.midi(n) unless octave
      # the octave: opt sets the octave outright, whatever the name had (Note.resolve_midi_note)
      raise InvalidOctaveError, "Invalid octave: #{octave.inspect}, expecting a number" unless octave.is_a?(Numeric)
      (Note.midi(n) % 12) + (octave.to_i * 12) + 12
    end

    def chord(tonic_or_name, *opts)
      tonic = 0
      name = :minor
      if opts.size == 0 || (opts.size == 1 && opts[0].is_a?(Hash))
        name = tonic_or_name
      else
        tonic = tonic_or_name
        name = opts.shift
      end
      return [] unless tonic
      opts = resolve_synth_opts_hash_or_array(opts)
      c = if is_list_like?(tonic)
            raise "List passed as parameter to chord needs two elements i.e. (chord [:e3, :minor]), you passed: #{tonic.inspect}" unless tonic.size == 2
            Theory.chord(tonic[0], tonic[1], opts[:num_octaves])
          else
            Theory.chord(tonic, name, opts[:num_octaves])
          end
      return chord_invert(c, opts[:invert]).ring if opts[:invert]   # as native: the inversion, then the ring
      SonicPi::Ring.new(c)
    end

    def scale(tonic_or_name, name_or_opts = nil, opts = nil)
      if name_or_opts.nil? || name_or_opts.is_a?(Hash)
        tonic = 0
        name = tonic_or_name
        num_octaves = (name_or_opts.is_a?(Hash) && name_or_opts[:num_octaves]) || 1
      else
        tonic = tonic_or_name
        name = name_or_opts
        num_octaves = (opts.is_a?(Hash) && opts[:num_octaves]) || 1
      end
      SonicPi::Ring.new(Theory.scale(tonic, name, num_octaves))
    end

    # ── Synths ────────────────────────────────────────────────────────────

    def current_synth = __local(:synth)
    def current_synth_name = __local(:synth)

    def use_synth(synth_name, *args, &block)
      raise "use_synth does not accept opts such as #{arg_h_pp(resolve_synth_opts_hash_or_array(args))}. \n Consider using use_synth_defaults." unless args.empty?
      raise "use_synth does not work with a do/end block. Perhaps you meant with_synth" if block
      __set_local(:synth, synth_name)
    end

    def with_synth(synth_name, *args, &block)
      raise "with_synth does not accept opts such as #{arg_h_pp(resolve_synth_opts_hash_or_array(args))}. \n Consider using with_synth_defaults." unless args.empty?
      raise "with_synth must be called with a do/end block. Perhaps you meant use_synth" unless block
      __with_local(:synth, synth_name) { block.call }
    end

    def use_synth_defaults(*args, &block)
      raise "use_synth_defaults does not work with a block. Perhaps you meant with_synth_defaults" if block
      __set_local(:synth_defaults, resolve_synth_opts_hash_or_array(args).dup)
    end

    def use_merged_synth_defaults(*args, &block)
      raise "use_merged_synth_defaults does not work with a block. Perhaps you meant with_merged_synth_defaults" if block
      __set_local(:synth_defaults, (__local(:synth_defaults) || {}).merge(resolve_synth_opts_hash_or_array(args)))
    end

    def with_synth_defaults(*args, &block)
      raise "with_synth_defaults must be called with a do/end block" unless block
      __with_local(:synth_defaults, resolve_synth_opts_hash_or_array(args).dup) { block.call }
    end

    def with_merged_synth_defaults(*args, &block)
      raise "with_merged_synth_defaults must be called with a do/end block" unless block
      __with_local(:synth_defaults, (__local(:synth_defaults) || {}).merge(resolve_synth_opts_hash_or_array(args))) { block.call }
    end

    def current_synth_defaults = __local(:synth_defaults)

    def use_transpose(shift, &block)
      raise "use_transpose does not work with a do/end block. Perhaps you meant with_transpose" if block
      raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
      __set_local(:transpose, shift)
    end

    def with_transpose(shift, &block)
      raise "with_transpose requires a do/end block. Perhaps you meant use_transpose" unless block
      raise "Transpose value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
      __with_local(:transpose, shift) { block.call }
    end

    def use_octave(shift, &block)
      raise "use_octave does not work with a do/end block. Perhaps you meant with_octave" if block
      raise "Octave shift must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
      __set_local(:octave_shift, shift)
    end

    def with_octave(shift, &block)
      raise "with_octave requires a do/end block. Perhaps you meant use_octave" unless block
      raise "Octave shift must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
      __with_local(:octave_shift, shift) { block.call }
    end

    def use_cent_tuning(shift, &block)
      raise "use_cent_tuning does not work with a do/end block. Perhaps you meant with_cent_tuning" if block
      raise "Cent tuning value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
      __set_local(:cent_tuning, shift)
    end

    def with_cent_tuning(shift, &block)
      raise "with_cent_tuning requires a do/end block. Perhaps you meant use_cent_tuning" unless block
      raise "Cent tuning value must be a number, got #{shift.inspect}" unless shift.is_a?(Numeric)
      __with_local(:cent_tuning, shift) { block.call }
    end

    # the app's "Warn about unknown opts", on a run's first line as Safe mode's use_arg_checks is (__check_opts!)
    def __warn_unknown_opts(v) = __set_local(:warn_unknown_opts, !!v)

    def use_arg_checks(v, &block)
      raise "use_arg_checks does not work with a do/end block. Perhaps you meant with_arg_checks" if block
      __set_local(:check_synth_args, !!v)
    end

    def with_arg_checks(v, &block)
      raise "with_arg_checks requires a do/end block. Perhaps you meant use_arg_checks" unless block
      __with_local(:check_synth_args, v) { block.call }
    end

    def use_arg_bpm_scaling(v, &block)
      raise "use_arg_bpm_scaling does not work with a do/end block. Perhaps you meant with_arg_bpm_scaling" if block
      __set_local(:arg_bpm_scaling, v)
    end

    def with_arg_bpm_scaling(v, &block)
      raise "with_arg_bpm_scaling requires a do/end block. Perhaps you meant use_arg_bpm_scaling" unless block
      __with_local(:arg_bpm_scaling, v) { block.call }
    end

    def set_sched_ahead_time!(t = DEFAULT_SCHED_AHEAD)
      raise ArgumentError, "sched ahead time must be a number, got #{t.inspect}" unless t.is_a?(Numeric)
      SonicPi.sched_ahead = t.to_f   # the session's (set_ is global, as use_ is the thread's own)
    end

    # Native's use_real_time is "use_sched_ahead_time 0", so asking for a lead again is no longer real time.
    def use_sched_ahead_time(t)
      raise ArgumentError, "sched ahead time must be a number, got #{t.inspect}" unless t.is_a?(Numeric)
      __p.sched_ahead = t.to_f
      __set_local(:real_time, t.to_f == 0.0 ? true : nil)
    end

    def current_sched_ahead_time = __p.sched_ahead

    # An fx around a block (Scheduler's FxFrame): its synth starts, then the
    # block's sounds, and the threads it starts, go into it; it is freed once
    # they have all ended and its kill_delay has passed.
    def with_fx(fx_name, *args, &block)
      raise ArgumentError, "with_fx must be called with a do/end block" unless block
      raise "with_fx block must only accept 0 or 1 args" unless block.arity == 0 || block.arity == 1
      args_h = resolve_synth_opts_hash_or_array(args)
      args_h[:reps] = 1 unless args_h[:reps]
      reps = args_h[:reps]
      if fx_name == :none || !__should_trigger?(args_h)
        node = BlankNode.new(args_h)
        return reps.times { block.arity == 0 ? block.call : block.call(node) }
      end
      fx_name = fx_name.to_sym
      info = SonicPi::Data::FX[fx_name] or raise "Unknown FX #{fx_name.inspect}"
      __check_opts!("FX", fx_name, info, args_h, FX_OWN_OPTS)
      p = __p
      frame = @sched.open_fx(p)
      res = nil
      begin
        node = __trigger_fx(fx_name, args_h, frame)
        frame.kill_delay = __fx_kill_delay(fx_name, node.args, info)
        @sched.enter_fx(p, frame, node)
        reps.times { res = block.arity == 0 ? block.call : block.call(node) }
      ensure
        @sched.close_fx(p, frame)
      end
      res
    end

    # Sonic Pi's kill_delay: the opt, or as long as the fx's tail needs given
    # its opts as sent (so tempo-scaled), or 1.
    def __fx_kill_delay(name, args, info)
      return args[:kill_delay] if args[:kill_delay].is_a?(Numeric)
      d = info[:defaults] || {}
      case name
      when :reverb then [(args[:room] || d[:room]) * 10 + 1, 11].min
      when :gverb then args[:release] || d[:release]
      when :echo then args[:decay] || d[:decay]
      when :ping_pong then Math.log(0.01) / Math.log(args[:feedback] || d[:feedback]) * (args[:phase] || d[:phase])
      else info[:kill_delay] || 1
      end
    end

    # How long a synth sounds before its envelope frees it: attack, decay,
    # sustain and release, as sent. A sample player's sustain of -1 (its own
    # default) is the rest of its stretch of the buffer at its rate.
    def __node_lifetime(args_h, info, sample = nil)
      d = info[:defaults] || {}
      arg = lambda do |k, dflt = 0|
        v = args_h[k]
        v = d[k] if v.nil?
        v.is_a?(Numeric) ? v.to_f : dflt
      end
      attack, decay, release = arg.call(:attack), arg.call(:decay), arg.call(:release)
      sustain = arg.call(:sustain)
      if sample
        rate = arg.call(:rate, 1).abs
        return 1.0e9 if rate == 0
        length = sample.duration
        length *= (arg.call(:finish, 1) - arg.call(:start, 0)).abs unless info[:scsynth_name].include?("basic_")
        sustain = length / rate - attack - release - decay if args_h[:sustain].nil? || args_h[:sustain] == -1
      end
      attack + decay + (sustain > 0 ? sustain : 0) + release
    end

    def play(n, *args, &blk)
      raise "Play expects a note to play. If you want to make a sound with a sample or buffer, you can just use 'sample ...' or 'sample buffer(...)'" if n.is_a?(SynthNode)
      if n.is_a?(Hash) && args.empty?
        synth(nil, n, &blk)
      else
        synth(nil, { note: n }, *args, &blk)
      end
    end

    def synth(synth_name, *args, &blk)
      synth_name = current_synth unless synth_name
      sn_sym = synth_name.to_sym
      info = SonicPi::Data::SYNTHS[sn_sym]
      external = !info && __local(:external_synths)
      unless info || external
        raise "Unknown synth #{sn_sym.inspect}: it is a synthdef you loaded, so turn on Enable external synths in the Preferences (or use_external_synths true) to play it" if SonicPi.external_synthdefs&.key?(sn_sym)
        raise "Unknown synth #{sn_sym.inspect}"
      end
      info ||= __external_synth_info(sn_sym)   # use_external_synths: a loaded synthdef of its own (lang_more.rb)
      args_h = resolve_synth_opts_hash_or_array(args)
      tls = __local(:synth_defaults) || {}
      args_h = tls.merge(args_h)
      __check_opts!("synth", sn_sym, info, args_h, SYNTH_OWN_OPTS) unless external
      if Note.rest?(args_h[:note]) && (args_h.key?(:note) || !args_h.key?(:notes)) && (!external || args_h.key?(:note))   # an external synth may take no note at all (freq:, say): only an explicit rest rests
        if args_h.key?(:note) || args.empty?
          __log("synth #{sn_sym.inspect}, {note: :rest}") unless __local(:synth_silent)
          return BlankNode.new(args_h)
        end
      end
      return __trigger_inst(sn_sym, args_h, info) if external   # its opts as given: no notes, chords or tuning of ours
      notes = args_h[:notes] || args_h[:note]
      if is_list_like?(notes)
        args_h.delete(:notes)
        args_h.delete(:note)
        shifted = notes.map { |x| __resolve_note(x, args_h) }
        return __trigger_chord(sn_sym, shifted, args_h)
      end
      n = args_h[:note] || info[:defaults][:note] || 52
      args_h[:note] = __resolve_note(n, args_h)
      res_node = __trigger_inst(sn_sym, args_h, info)
      if blk
        in_thread { blk.call(res_node) }
      end
      res_node
    end

    def play_chord(notes, *args)
      raise "play_chord expects a list of notes such as [70, 75, 82], got #{notes.inspect}" unless is_list_like?(notes)
      args_h = resolve_synth_opts_hash_or_array(args)
      shifted = notes.map { |x| __resolve_note(x, args_h) }
      __trigger_chord(current_synth_name.to_sym, shifted, args_h)
    end

    def play_pattern_timed(notes, times, *args)
      if is_list_like?(times)
        t = times.to_a.ring
        opts = args.last.is_a?(Hash) ? args.last : {}
        explicit_length = opts.key?(:sustain) || opts.key?(:duration)
        match_total = !explicit_length && [:attack, :decay, :release, :attack_level, :decay_level, :sustain_level, :env_curve].any? { |o| opts.key?(o) }
        notes.each_with_index do |n, idx|
          duration = t[idx]
          kwargs = opts.dup
          kwargs.each { |k, v| kwargs[k] = v.to_a.ring[idx] if is_list_like?(v) }
          if match_total
            kwargs[:duration] = duration
          elsif !explicit_length
            kwargs[:sustain] = duration
          end
          play(n, kwargs)
          sleep(duration)
        end
      else
        play_pattern_timed(notes, [times], *args)
      end
    end

    def play_pattern(notes, *args) = play_pattern_timed(notes, 1, *args)

    def __resolve_note(n, args_h)
      n = n || args_h[:note]
      n = n.call if n.is_a?(Proc)
      n = n[0] if is_list_like?(n) && n.size == 1
      n = note(n) unless n.is_a?(Numeric)
      n += __local(:transpose) if __local(:transpose)
      n += 12 * __local(:octave_shift) if __local(:octave_shift)
      n += __local(:cent_tuning) / 100.0 if __local(:cent_tuning)
      n += @sched.cent_tuning / 100.0     # the studio's cent tuning (set_cent_tuning!)
      n += args_h[:pitch].to_f
      tuning = __local(:tuning)
      n = SonicPi::Tuning.resolve(n, tuning[0], tuning[1]) if tuning && tuning[0] != :equal
      n
    end

    # use_timing_guarantees: a sound that would arrive late is not played, and says so, as native's in_good_time?
    # has it (a thread more than 1.1s behind is an error there too)
    def __out_of_time?(what)
      return false unless __local(:timing_guarantees)
      behind = @sched.behind(__p)
      return false if behind < 0
      raise "Timing Exception: thread got too far behind time." if behind >= 1.1
      __log("!! Out of time, skipping: #{what}")
      true
    end

    def __trigger_inst(sn, args_h, info)
      processed = __normalise_and_resolve(args_h, info, true)
      return BlankNode.new(args_h) if __out_of_time?("synth #{sn.inspect}, #{arg_h_pp(processed)}")
      __log("synth #{sn.inspect}, #{arg_h_pp(processed)}") unless __local(:synth_silent)
      __add_slide_times!(processed, info)
      __trigger_synth(sn, processed, info)
    end

    def __trigger_chord(sn, notes, args_h)
      info = SonicPi::Data::SYNTHS[sn]
      args_h = __normalise_and_resolve(resolve_synth_opts_hash_or_array(args_h), info, true)
      return BlankNode.new(args_h) if __out_of_time?("synth #{sn.inspect}, #{arg_h_pp({ note: notes }.merge(args_h))}")
      __log("synth #{sn.inspect}, #{arg_h_pp({ note: notes }.merge(args_h))}") unless __local(:synth_silent)
      amp = args_h[:amp] || 1.0
      args_h[:amp] = amp.to_f / notes.size
      nodes = []
      notes.each do |n|
        next unless n
        args_h[:note] = n
        nodes << __trigger_synth(sn, args_h.dup, info)
      end
      group = ChordGroup.new(nodes, notes, info)
      __set_local(:last_node, group)
      group
    end

    def __trigger_fx(name, args_h, frame = nil)
      info = SonicPi::Data::FX[name] or raise "Unknown FX #{name.inspect}"
      args_h = args_h.dup
      args_h[:reps] = 1 unless args_h.key?(:reps)     # before the tempo-scaled args, where Sonic Pi puts it
      args_h = __normalise_and_resolve(args_h, info, false)
      __add_slide_times!(args_h, info)
      __trigger_synth(name, args_h, info, !info[:logical_clock], false, frame)   # most fx start now; some ride the logical clock
    end

    # track: a synth or sample the program may control without naming it
    # (control with no node takes the last one); an fx is not. frame: the
    # with_fx frame, when this is its fx; sample: the buffer a player plays.
    def __trigger_synth(sn, args_h, info, now = false, track = true, frame = nil, sample = nil)
      scsynth = info[:scsynth_name]
      __validate!(info, args_h, sn) if __local(:check_synth_args)
      return BlankNode.new(args_h) unless __should_trigger?(args_h)
      recorded = {}
      args_h.each { |k, v| recorded[k.to_s] = v }
      p = __p
      id = @sched.next_node_id
      @sched.record_event(p, { synth: scsynth, args: recorded, now: now }, id, frame)
      node = SynthNode.new(scsynth, args_h, info, id, { t: p.time.round(6), thread: p.id, sa: p.sched_ahead })   # sa: its thread's, for a trace's frame (Scheduler#trace_ref)
      if frame
        node.frame = frame
      else
        node.ends_at = p.start + p.time + p.sched_ahead + __node_lifetime(args_h, info, sample)   # on the session's clock
      end
      @sched.track(p, node)
      __set_local(:last_node, node) if track
      node
    end

    def __should_trigger?(args_h)
      return true unless args_h.key?(:on)
      truthy?(args_h.delete(:on))
    end

    # the opts a call takes over its synth's own: those play/synth, with_fx and sample handle themselves
    SYNTH_OWN_OPTS = [:note, :notes, :on, :slide, :pitch, :duration].freeze
    FX_OWN_OPTS = [:reps, :kill_delay, :on, :slide].freeze
    SAMPLE_OWN_OPTS = [:on, :slide, :pitch, :duration, :rate, :beat_stretch, :pitch_stretch, :rpitch, :onset, :slice,
                       :num_slices, :path, :start, :finish, :norm, :lpf_min, :hpf_max, :window_size, :pitch_dis, :time_dis].freeze

    # every opt any synth, FX or sample takes
    def self.all_opts
      @all_opts ||= begin
        h = {}
        [SonicPi::Data::SYNTHS, SonicPi::Data::FX].each { |t| t.each_value { |i| i[:defaults].each_key { |k| h[k] = true }; (i[:aliases] || {}).each_key { |k| h[k] = true } } }
        (SYNTH_OWN_OPTS + FX_OWN_OPTS + SAMPLE_OWN_OPTS).each { |k| h[k] = true }
        h
      end
    end

    # With the app's "Warn about unknown opts" on (its Preferences, apart from Safe mode: native says nothing of an opt
    # a synth does not have): an opt that no synth, FX or sample takes is most likely a misspelling (cutof:), which
    # would otherwise do nothing, silently. It is still ignored, as Sonic Pi ignores it (the tutorial says so), and the
    # code plays on, but it is said: a warning record the app shows with its did-you-mean, once for each opt on each
    # line of a run. One some other synth takes (cutoff: to :beep) is let by without a word: code passes such opts to
    # whichever synth is playing.
    def __check_opts!(kind, name, info, args_h, own)
      return unless __local(:warn_unknown_opts) && info && !info[:external]
      args_h.each_key do |k|
        next if own.include?(k) || info[:defaults].key?(k) || (info[:aliases] || {}).key?(k) || Language.all_opts.key?(k)
        p = __p
        seen = (@warned_opts ||= {})
        key = "#{p.file}:#{@sched.line_of(p)}:#{k}"
        next if seen[key]
        seen[key] = true
        seen.delete(seen.keys.first) while seen.size > 500
        message = "Unknown opt #{k}: for #{kind} #{name.inspect}"
        __log("#{message} - ignored")
        @sched.rt_record(p, { kind: "warning", class: "UnknownOpt", message: message })
      end
    end

    # Safe mode's checks. The rules are native's own (runtime/data/synths.rb, from its SynthInfo) and so is the
    # judgement: validation.rb is native's file, copied here by scripts/gen-synth-data.rb, and native calls the same
    # one through its lambdas. Nothing here interprets a rule; the words come from the rule too.
    def __validate!(info, args_h, name = nil)
      rules = info[:rules]
      return unless rules
      args_h.each do |arg, v|
        (rules[arg] || []).each do |rule|
          next if SonicPi::Validation.ok?(rule, v, args_h)
          # the opt as the program wrote it, where it was renamed on the way in (cutoff: is lpf: on the sample
          # players): the message is native's, which names the opt the synth has, and the fault carries both so the
          # error card can point at the word that is actually in the code
          wrote = nil
          (info[:aliases] || {}).each { |from, to| wrote = from if to == arg }
          raise SonicPi::Validation.error(arg, rule, v, { as: wrote, synth: name })
        end
      end
    end

    def __normalise_and_resolve(args_h, info, combine_tls)
      args_h = args_h.dup
      args_h.delete_if { |_, v| v.nil? }
      defaults = info ? info[:defaults] : {}
      if combine_tls
        (__local(:synth_defaults) || {}).each { |k, v| args_h[k] = v unless args_h.key?(k) || v.nil? }
      end
      __alias_opts!(args_h, info) if info
      __resolve_midi_args!(args_h, info) if info
      __normalise_args!(args_h, defaults)
      __calculate_sustain!(args_h, defaults)
      __scale_time_args!(args_h, info) if info && __local(:arg_bpm_scaling)
      args_h
    end

    # an opt that is a pitch (cutoff:, lpf:, centre: …, the synth's own list) takes a note name as play does:
    # cutoff: :e5 is 76, as native's resolve_midi_args! has it (synths, fx and control; a sample's are not)
    def __resolve_midi_args!(args_h, info)
      (info[:midi_args] || []).each { |a| args_h[a] = note(args_h[a]) if args_h.key?(a) }
      args_h
    end

    def __normalise_args!(args_h, defaults)
      args_h.keys.each do |k|
        v = args_h[k]
        v = v.call if v.is_a?(Proc)
        case v
        when Numeric then next
        when Symbol then args_h[k] = (args_h[v] || defaults[v]).to_f
        when TrueClass then args_h[k] = 1.0
        when FalseClass then args_h[k] = 0.0
        when NilClass then args_h[k] = nil
        else
          begin
            args_h[k] = v.to_f
          rescue
            raise "Unable to normalise argument with key #{k.inspect} and value #{v.inspect}"
          end
        end
      end
    end

    def __calculate_sustain!(args, defaults)
      if args.key?(:duration) && !args.key?(:sustain)
        attack = args.fetch(:attack, defaults.fetch(:attack, 0))
        decay = args.fetch(:decay, defaults.fetch(:decay, 0))
        release = args.fetch(:release, defaults.fetch(:release, 0))
        sustain = args[:duration] - (attack + decay + release)
        args[:sustain] = [0, sustain].max
        args.delete(:duration)
      end
    end

    # Times are beats: at any tempo but 60 they reach the server in seconds,
    # and a default that changes under the tempo is sent too.
    def __scale_time_args!(args_h, info)
      defaults = info[:defaults]
      mul = __p.sleep_mul
      new_args = {}
      info[:bpm_scale_args].each do |arg|
        val = args_h.key?(arg) ? args_h[arg] : defaults[arg]
        val = (args_h[val] || defaults[val]) if val.is_a?(Symbol)
        scaled = val * mul
        if args_h.key?(arg)
          new_args[arg] = scaled
        else
          new_args[arg] = scaled unless defaults[arg] == scaled
        end
      end
      new_args.each { |k, v| args_h[k] = v }
    end

    def __add_slide_times!(args_h, info)
      slide = args_h[:slide]
      return unless slide
      info[:slide_args].each { |k| args_h[k] = slide unless args_h.key?(k) }
    end


    # ── Samples ───────────────────────────────────────────────────────────

    SIMPLE_SAMPLER_ARGS = [:amp, :amp_slide, :amp_slide_shape, :amp_slide_curve, :pan, :pan_slide, :pan_slide_shape, :pan_slide_curve,
                           :cutoff, :cutoff_slide, :cutoff_slide_shape, :cutoff_slide_curve, :lpf, :lpf_slide, :lpf_slide_shape, :lpf_slide_curve,
                           :hpf, :hpf_slide, :hpf_slide_shape, :hpf_slide_curve, :rate, :slide, :beat_stretch, :rpitch, :attack, :decay,
                           :sustain, :release, :attack_level, :decay_level, :sustain_level, :env_curve]

    def pitch_to_ratio(m) = 2.0 ** (m.to_f / 12.0)
    def ratio_to_pitch(r) = 12.0 * Math.log2(r.abs.to_f)

    def __sample_split(args)
      idx = args.index { |el| el.is_a?(Hash) }
      if idx
        filts = args[0, idx]
        opts = args[idx..]
      else
        filts = args
        opts = {}
      end
      filts = filts.map { |f| f.is_a?(Samples::Info) ? f.path : f }.compact
      [filts, opts]
    end

    def sample_find_candidates(*args) = Samples.find_candidates(args)
    def resolve_sample_paths(filts) = Samples.find_candidates(filts)
    def resolve_sample_path(filts) = resolve_sample_paths(filts)[0]

    def sample_paths(*args)
      filts, _ = __sample_split(args)
      SonicPi::Ring.new(resolve_sample_paths(filts))
    end

    def __load_sample_at_path(path)
      raise "Unknown sample description: #{path.inspect}\n expected a string containing a path." unless path.is_a?(String)
      raise "Attempted to load sample with an empty string as path" if path.empty?
      path = Samples.expand(path)
      info = Samples.info(path) or raise "Could not find sample: #{path}"
      unless Samples.loaded?(path)
        Samples.mark_loaded(path)
        @sched.record_load(__p, Samples.basename(path))
      end
      info
    end

    def sample_loaded?(*args)
      filts, _ = __sample_split(args)
      path = resolve_sample_path(filts)
      Samples.loaded?(Samples.expand(path))
    end

    def load_sample(*args)
      filts, _ = __sample_split(args)
      __load_sample_at_path(sample_find_candidates(filts)[0])
    end

    # load_samples takes names, and lists and rings of names (load_samples sample_names(:ambi)), each loaded on
    # its own, as Sonic Pi does (lang/sound.rb load_samples); a lone filter set loads every match.
    def load_samples(*args)
      if args.any? { |a| a.is_a?(Array) || a.is_a?(SonicPi::Ring) }
        args.each { |a| a.is_a?(Array) || a.is_a?(SonicPi::Ring) ? load_samples(*a.to_a) : load_sample(a) }
        return nil
      end
      filts, _ = __sample_split(args)
      sample_find_candidates(filts).map { |p| __load_sample_at_path(p) }
    end

    def sample_buffer(*args)
      filts, _ = __sample_split(args)
      __load_sample_at_path(resolve_sample_path(filts))
    end
    alias_method :sample_info, :sample_buffer

    def sample_duration(*args)
      filts, args_a = __sample_split(args)
      path = resolve_sample_path(filts)
      raise ArgumentError, "Error calling sample_duration: filters matched no samples" if path.nil?
      dur = __load_sample_at_path(path).duration
      args_h = merge_synth_arg_maps_array(args_a)
      __normalise_sample_args(path, args_h, nil, true)
      start = [1, [0, args_h[:start] || 0].max].min
      finish = [1, [0, args_h[:finish] || 1].max].min
      rate = args_h[:rate] || 1
      len = finish > start ? finish - start : start - finish
      real_dur = dur * 1.0 / rate.abs * len
      if args_h.key?(:sustain) && args_h[:sustain] != -1
        attack = [0, args_h[:attack].to_f].max
        decay = [0, args_h[:decay].to_f].max
        sustain = [0, args_h[:sustain].to_f].max
        release = [0, args_h[:release].to_f].max
        real_dur = [attack + decay + sustain + release, real_dur].min
      end
      __local(:arg_bpm_scaling) ? real_dur.to_f / __p.sleep_mul : real_dur
    end

    def sample_names(group)
      g = SonicPi::Data::SAMPLE_GROUPS[group.to_sym] or raise "Unknown sample group #{group.inspect}"
      SonicPi::Ring.new(g[:samples].sort)
    end

    def all_sample_names = SonicPi::Ring.new(SonicPi::Data::SAMPLE_GROUPS.values.map { |g| g[:samples] }.flatten.sort)
    def sample_groups = SonicPi::Ring.new(SonicPi::Data::SAMPLE_GROUPS.keys.sort)

    def use_sample_defaults(*args, &block)
      raise "use_sample_defaults does not work with a block. Perhaps you meant with_sample_defaults" if block
      __set_local(:sample_defaults, resolve_synth_opts_hash_or_array(args).dup)
    end

    def use_merged_sample_defaults(*args, &block)
      raise "use_merged_sample_defaults does not work with a block. Perhaps you meant with_merged_sample_defaults" if block
      __set_local(:sample_defaults, (__local(:sample_defaults) || {}).merge(resolve_synth_opts_hash_or_array(args)))
    end

    def with_sample_defaults(*args, &block)
      raise "with_sample_defaults must be called with a do/end block" unless block
      __with_local(:sample_defaults, resolve_synth_opts_hash_or_array(args).dup) { block.call }
    end

    def with_merged_sample_defaults(*args, &block)
      raise "with_merged_sample_defaults must be called with a do/end block" unless block
      __with_local(:sample_defaults, (__local(:sample_defaults) || {}).merge(resolve_synth_opts_hash_or_array(args))) { block.call }
    end

    def current_sample_defaults = __local(:sample_defaults)

    def use_sample_bpm(sample_name, *args)
      args_h = resolve_synth_opts_hash_or_array(args)
      num_beats = args_h[:num_beats] || 1
      sd = __with_local(:arg_bpm_scaling, false) { sample_duration(sample_name, *args) }
      use_bpm(num_beats * (60.0 / sd))
    end

    def with_sample_bpm(sample_name, *args, &block)
      raise "with_sample_bpm must be called with a do/end block" unless block
      args_h = resolve_synth_opts_hash_or_array(args)
      num_beats = args_h[:num_beats] || 1
      sd = sample_buffer(sample_name).duration
      with_bpm(num_beats * (60.0 / sd), &block)
    end

    def __filts_for_log(filts)
      SonicPi.log_inspect(filts.map { |f| f.is_a?(String) && Samples.builtin_dir && f.start_with?(Samples.builtin_dir) ? "<samples>" + f[Samples.builtin_dir.size..] : f })
    end

    def sample(*args, &blk)
      filts, args_a = __sample_split(args)
      args_h = merge_synth_arg_maps_array(args_a)
      args_h = (__local(:sample_defaults) || {}).merge(args_h)
      if filts.size == 0
        if args_h.key?(:path)
          path = resolve_sample_path([args_h.delete(:path)])
        else
          __log("sample #{__filts_for_log(filts)}\n           - no match found, skipping.")
          return BlankNode.new(args_h)
        end
      else
        path = resolve_sample_path(filts)
      end
      if path.nil?
        # with the arg checks on (the app's runs, as native's GUI turns them on), a sample named that there is none of
        # is an error, not a skip: a misspelt name is the likelier story than a search meant to find nothing
        raise "Unknown sample #{filts[0].inspect}" if __local(:check_synth_args) && filts.size == 1 && filts[0].is_a?(Symbol)
        __log("sample #{__filts_for_log(filts)}\n           - no match found, skipping.")
        return BlankNode.new(args_h)
      end
      __check_opts!("sample", filts[0].is_a?(Symbol) ? filts[0] : :sample, SonicPi::Data::SYNTHS[:stereo_player], args_h, SAMPLE_OWN_OPTS)
      if Samples.loaded?(Samples.expand(path))
        res_node = __trigger_sampler(path, args_h)
      else
        # Sonic Pi loads a new sample in a thread of its own and plays it
        # from there; the thread's path is part of what programs can see.
        res_node = nil
        in_thread { res_node = __trigger_sampler(path, args_h) }
      end
      in_thread { blk.call(res_node) } if blk
      res_node
    end

    def __complex_sampler_args?(args_h)
      return false if args_h.empty?
      !(args_h.keys - SIMPLE_SAMPLER_ARGS).empty?
    end

    def __trigger_sampler(path, args_h)
      args_h = args_h.dup
      info = __load_sample_at_path(path)
      sn = if __complex_sampler_args?(args_h)
             info.num_chans == 1 ? :mono_player : :stereo_player
           else
             info.num_chans == 1 ? :basic_mono_player : :basic_stereo_player
           end
      synth_info = SonicPi::Data::SYNTHS[sn]
      args_h = __normalise_sample_args(path, args_h, synth_info)
      return BlankNode.new(args_h) if __out_of_time?(args_h.empty? ? "sample #{Samples.basename(path).inspect}" : "sample #{Samples.basename(path).inspect}, #{arg_h_pp(args_h)}")
      dir = Samples.dirname(path)
      dir = "<samples>" if dir == Samples.builtin_dir     # the trace never carries a machine path
      if args_h.empty?
        __log("sample #{dir.inspect},\n           #{Samples.basename(path).inspect}") unless __local(:synth_silent)
      else
        __log("sample #{dir.inspect},\n           #{Samples.basename(path).inspect}, #{arg_h_pp(args_h)}") unless __local(:synth_silent)
      end
      __add_slide_times!(args_h, synth_info)
      args_h[:buf] = Samples.basename(path)
      __trigger_synth(sn, args_h, synth_info, false, true, nil, info)
    end

    def __normalise_sample_args(path, args_h, info, combine_tls = false)
      args_h.delete_if { |_, v| v.nil? }
      defaults = info ? info[:defaults] : {}
      (__local(:sample_defaults) || {}).each { |k, v| args_h[k] = v unless args_h.key?(k) || v.nil? }
      if (stretch = args_h[:beat_stretch])
        raise "beat_stretch: opt needs to be a positive number. Got: #{stretch.inspect}" unless stretch.is_a?(Numeric) && stretch > 0
        rate = args_h[:rate] || 1
        dur = sample_buffer(path).duration
        args_h[:rate] = (1.0 / stretch.to_f) * rate * (current_bpm / (60.0 / dur))
      end
      if (pstretch = args_h[:pitch_stretch])
        raise "pitch_stretch: opt needs to be a positive number. Got: #{pstretch.inspect}" unless pstretch.is_a?(Numeric) && pstretch > 0
        rate = args_h[:rate] || 1
        dur = sample_buffer(path).duration
        new_rate = (1.0 / pstretch.to_f) * (current_bpm / (60.0 / dur))
        args_h[:rate] = new_rate * rate
        args_h[:pitch] = args_h[:pitch].to_f - ratio_to_pitch(new_rate)
      end
      if (rpitch = args_h[:rpitch])
        args_h[:rate] = pitch_to_ratio(rpitch.to_f) * (args_h[:rate] || 1)
      end
      orig_start = args_h[:start] || 0
      orig_finish = args_h[:finish] || 1
      onset_idx = args_h[:onset]
      onset_start = onset_finish = nil
      if onset_idx
        onsets = sample_buffer(path).onset_slices
        if onset_idx.is_a?(Numeric)
          onset = onsets[onset_idx.round]
        elsif onset_idx.is_a?(Proc)
          onset = onset_idx.call(onsets)
          onset = onset[0] if is_list_like?(onset)
          raise "Result of onset: proc should be a Map such as {:start => 0, :finish => 0.125}. Got: #{onset.inspect}" unless onset.respond_to?(:key?) && onset[:start].is_a?(Numeric) && onset[:finish].is_a?(Numeric)
        else
          raise "Unknown sample onset: value. Expected a number or a proc. Got #{onset_idx.inspect}"
        end
        onset_dur = onset[:finish] - onset[:start]
        onset_start = onset[:start]
        onset_finish = onset[:finish]
        args_h[:start] = onset_start + (orig_start * onset_dur)
        args_h[:finish] = [onset_start + (orig_finish * onset_dur), 1].min
        args_h[:onset] = onset[:index] if onset[:index]
      end
      if (slice_idx = args_h[:slice])
        num_slices = args_h.fetch(:num_slices, 16).round
        raise "Sample opt num_slices: needs to be greater than 0. Got: #{num_slices}" unless num_slices.is_a?(Numeric) && num_slices > 0
        slices = onset_idx ? sample_buffer(path).slices(num_slices, onset_start, onset_finish) : sample_buffer(path).slices(num_slices)
        if slice_idx.is_a?(Numeric)
          slice = slices[slice_idx.round]
        elsif slice_idx.is_a?(Proc)
          slice = slice_idx.call(slices)
          slice = slice[0] if is_list_like?(slice)
          raise "Result of slice: proc should be a Map such as {:start => 0, :finish => 0.125}. Got: #{slice.inspect}" unless slice.respond_to?(:key?) && slice[:start].is_a?(Numeric) && slice[:finish].is_a?(Numeric)
        else
          raise "Unknown sample slice: value. Expected a number or a proc. Got #{slice_idx.inspect}"
        end
        slice_dur = slice[:finish] - slice[:start]
        args_h[:start] = slice[:start] + (orig_start * slice_dur)
        args_h[:finish] = [slice[:start] + (orig_finish * slice_dur), 1].min
        args_h[:slice] = slice[:index] if slice[:index]
      end
      __alias_opts!(args_h, info) if info
      __normalise_args!(args_h, defaults)
      __calculate_sustain!(args_h, defaults)
      __scale_time_args!(args_h, info) if info && __local(:arg_bpm_scaling)
      args_h
    end

    # A synth's opt aliases (cutoff: for lpf: on the sample players), as
    # its munge_opts applies them.
    def __alias_opts!(args_h, info)
      (info[:aliases] || {}).each do |from, to|
        if args_h.key?(from) && !args_h.key?(to)
          args_h[to] = args_h[from]
          args_h.delete(from)
        end
      end
    end

    # ── Not in the web build ──────────────────────────────────────────────

    # There is no OSC on the web: these say so, rather than failing as
    # undefined methods.
    def __no_osc(name) = raise("#{name} is never on the web: a browser cannot send or receive OSC (it travels over UDP)")
    def osc(*_args) = __no_osc(:osc)

    # native's tracks (v6.0) host audio plugins, which only the desktop app can run: each says so, and the two that
    # only ask answer truly (there are no tracks here, and no current one)
    NO_TRACKS = "tracks host audio plugins (Surge and the like), which only the desktop app can run"
    def __no_tracks(name) = raise("#{name} is never on the web: #{NO_TRACKS}")
    def tracks = SonicPi::Ring.new([])
    def current_track = nil
    %i[live_track use_track with_track with_send track_midi track_midi_note_on track_midi_note_off track_midi_cc
       track_midi_pitch_bend track_midi_all_notes_off track_control].each do |fn|
      define_method(fn) { |*_args, &_block| __no_tracks(fn) }
    end
    def osc_send(*_args) = __no_osc(:osc_send)
    def use_osc(*_args) = __no_osc(:use_osc)
    def with_osc(*_args, &_block) = __no_osc(:with_osc)
    def use_osc_logging(*_args) = __no_osc(:use_osc_logging)
    def with_osc_logging(*_args, &_block) = __no_osc(:with_osc_logging)

    # ── Debug, real time, pitch conversions ───────────────────────────────

    # use_debug false silences the synth and sample echoes, as in Sonic Pi.
    def use_debug(v, &block)
      raise "use_debug does not work with a do/end block. Perhaps you meant with_debug" if block
      __set_local(:synth_silent, !v)
    end

    def with_debug(v, &block)
      raise "with_debug requires a do/end block. Perhaps you meant use_debug" unless block
      __with_local(:synth_silent, !v) { block.call }
    end

    # This thread schedules with no ahead time.
    #
    # Both of native's thread locals, as native sets them (core.rb use_real_time): the ahead time, and the flag
    # saying this thread asked for none. The flag is not the same fact twice — a sound from such a thread leaves
    # at the moment it was asked for, so it is always a shade past its time by the time it goes, and native
    # exempts exactly these from every lateness check it makes (sound.rb, "unless real_time_mode"). Without the
    # flag nothing downstream can tell a sound that was never given any lead from one that lost it.
    def use_real_time
      __p.sched_ahead = 0.0
      __set_local(:real_time, true)
      nil
    end

    def hz_to_midi(freq) = (12 * (Math.log(freq * 0.0022727272727) / Math.log(2))) + 69

    def midi_to_hz(n)
      midi_num = note(n)
      raise "Invalid argument to midi_to_hz. Expected a note or a number, got: #{n.inspect}" unless midi_num.is_a?(Numeric)
      440.0 * (2 ** ((midi_num - 69) / 12.0))
    end

    # ── Helpers from Sonic Pi's util ──────────────────────────────────────

    def is_list_like?(o) = o.is_a?(Array) || o.is_a?(SonicPi::Ring)

    def resolve_synth_opts_hash_or_array(opts)
      case opts
      when Hash then opts
      when Array, SonicPi::Ring then merge_synth_arg_maps_array(opts)
      when NilClass then {}
      else raise "Invalid options. Options should either be an even list of key value pairs, a single Hash or nil. Got #{opts.inspect}"
      end
    end

    def merge_synth_arg_maps_array(opts_a)
      return opts_a if opts_a.is_a?(Hash)
      opts_a = opts_a.to_a
      res = {}
      idx = 0
      size = opts_a.size
      while idx < size && opts_a[idx].is_a?(Hash)
        res = res.merge(opts_a[idx])
        idx += 1
      end
      return res if idx == size
      left = opts_a[idx..]
      raise "There must be an even number of trailing synth args" unless left.size.even?
      h = {}
      (0...left.size).step(2) { |i| h[left[i]] = left[i + 1] }
      res.merge(h)
    end

    def split_params_and_merge_opts_array(opts_a)
      return [[], opts_a] if opts_a.is_a?(Hash)
      opts_a = opts_a.to_a
      params = []
      idx = 0
      while idx < opts_a.size && !opts_a[idx].is_a?(Hash)
        params << opts_a[idx]
        idx += 1
      end
      return [params, {}] if idx == opts_a.size
      [params, merge_synth_arg_maps_array(opts_a[idx..])]
    end

    def truthy?(val)
      case val
      when Numeric then val != 0
      when NilClass, FalseClass then false
      when Proc then truthy?(val.call)
      else true
      end
    end

    def arg_h_pp(arg_h)
      s = "{"
      arg_h.each do |k, v|
        next unless v
        rounded = v.is_a?(Float) ? SonicPi::FloatFormat.to_s(v.round(4)) : SonicPi.log_inspect(v)
        s += "#{k}: #{rounded}, "
      end
      s = s[0, s.size - 2] if s.end_with?(", ")
      s + "}"
    end
  end
end
