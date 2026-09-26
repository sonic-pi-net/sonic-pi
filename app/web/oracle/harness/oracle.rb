# SPDX-License-Identifier: AGPL-3.0-or-later
# The oracle: Sonic Pi's own Ruby runtime, run headless over a studio that
# records instead of playing. A program goes in; what the engine would have
# been told comes out, with the logical time of every event — the behaviour
# a new runtime must reproduce.
#
#   ruby oracle/harness/oracle.rb program.rb        # JSON trace on stdout
#
# Time is Sonic Pi's spider time (seconds, exact rationals) and beat, so the
# trace is deterministic apart from the order of events at one identical
# instant, which the comparison sorts.
require 'json'
require 'set'

SP_RUBY = File.expand_path("../../../../app/server/ruby", __dir__)
$LOAD_PATH.unshift File.join(SP_RUBY, "lib")
require File.join(SP_RUBY, "core")
require File.join(SP_RUBY, "lib/sonicpi/runtime")
require File.join(SP_RUBY, "lib/sonicpi/lang/core")
require File.join(SP_RUBY, "lib/sonicpi/lang/midi")
require File.join(SP_RUBY, "lib/sonicpi/lang/western_theory")
require File.join(SP_RUBY, "lib/sonicpi/lang/sound")
require File.join(SP_RUBY, "lib/sonicpi/event_history")
# the defaults the runtime keeps, kept here too, so a trace is taken under the settings the runtime is held to: the
# schedule-ahead (SonicPi::DEFAULT_SCHED_AHEAD), said once, in the runtime
require_relative "../../runtime/lib/sonic_pi/defaults"
require File.join(SP_RUBY, "lib/sonicpi/thread_id")
require File.join(SP_RUBY, "lib/sonicpi/buffer")
require File.join(SP_RUBY, "lib/sonicpi/samplebuffer")

# Sonic Pi finds aubio_onset in its own build output; ours is built by
# scripts/build-oracle.sh into oracle/bin, and the submodule stays untouched.
module SonicPi::Paths
  def self.aubio_onset_path = File.expand_path("../bin/aubio_onset", __dir__)
end

# Frame count, channels and rate from a sound file's header: FLAC STREAMINFO
# or a RIFF WAVE fmt/data pair. No decoder, no external tool.
# Sonic Pi keeps logical time as a float of the wall clock, about 1.8e9 s
# since 1970, where a double cannot hold a microsecond: each sleep rounds
# away a quarter of one, and a trace drifts by a millionth of a second every
# few beats. That is the size of the clock, not the language, so the
# oracle's clock starts near zero: Time.now counts from just before the
# harness started. The runtime only ever subtracts and compares clock
# readings, so nothing else it does changes.
ORACLE_CLOCK_EPOCH = Time.now.to_i - 10
class << Time
  alias_method :__oracle_real_now, :now
  def now(*args, **kw) = __oracle_real_now(*args, **kw) - ORACLE_CLOCK_EPOCH
end

module Oracle; end
module Oracle::AudioMeta
  def self.read(path)
    File.open(path, "rb") do |f|
      magic = f.read(4)
      return flac(f) if magic == "fLaC"
      return wav(f) if magic == "RIFF"
      raise "unknown sound file: #{path}"
    end
  end

  def self.flac(f)
    header = f.read(4).unpack1("N")          # 1 bit last, 7 bits type, 24 bits length
    raise "no STREAMINFO" unless (header >> 24) & 0x7f == 0
    info = f.read(header & 0xffffff)
    bits = info.byteslice(10, 8).unpack1("Q>")   # rate 20 | chans-1 3 | bps-1 5 | total samples 36
    { "sample_rate" => bits >> 44, "num_chans" => ((bits >> 41) & 0x7) + 1, "num_frames" => bits & ((1 << 36) - 1) }
  end

  def self.wav(f)
    f.read(4); raise "not WAVE" unless f.read(4) == "WAVE"
    meta = {}
    until f.eof?
      id, size = f.read(8)&.unpack("a4V") rescue break
      break unless id
      if id == "fmt "
        fmt = f.read(size)
        meta["num_chans"] = fmt.unpack1("v", offset: 2)
        meta["sample_rate"] = fmt.unpack1("V", offset: 4)
        meta["block_align"] = fmt.unpack1("v", offset: 12)
      elsif id == "data"
        meta["num_frames"] = size / meta.fetch("block_align")
        break
      else
        f.seek(size + (size & 1), IO::SEEK_CUR)
      end
    end
    meta.slice("sample_rate", "num_chans", "num_frames")
  end
end

module Oracle
  SAMPLES_DIR = File.expand_path("../../../../etc/samples", __dir__)
  # Programs that need a folder of samples refer to it by this constant, so a
  # spec never carries a machine path (see specs/README.md).
  Object.const_set(:SAMPLES_DIR, SAMPLES_DIR)

  # What a triggered synth looks like to the language: enough of a node.
  class Node < SonicPi::Node
    attr_reader :id, :name, :args
    # The trigger it came from (its time and thread), so a control or kill
    # names which synth it acted on; the studio records both.
    attr_accessor :ref, :studio, :info
    def initialize(id, name, args) = (@id, @name, @args = id, name, args)
    def on_next_move(&b) = nil
    # A node ends when the engine would free it, which the studio works out
    # (see RecordingStudio#lifetime); asked after that, the callback runs at
    # once. The job's mixer has no studio and ends at once.
    def on_destroyed(*, &b)
      return unless b
      @studio ? @studio.on_node_end(self, &b) : b.call
    end
    def sp_thread_safe? = true
    # Only what a program asked for with control or kill is recorded: the
    # runtime also stops its own nodes (an fx at the end of its block). A
    # program asks through the control and kill fns, or through the node
    # itself (s.control, s.ctl, s.kill), called from the program's own code:
    # evaluated as the "oracle" workspace (__spider_eval), one or two frames up.
    def self.from_program? = caller_locations(2, 3).any? { |l| l.path == "oracle" }
    def control(*args)
      return unless (Thread.current[:oracle_control] || Node.from_program?) && @studio
      args_h = args.size == 1 && args[0].is_a?(Hash) ? args[0] : Hash[*args]
      @studio.record(:control, synth: @name.to_s, of: @ref, args: @studio.program_args(args_h))
    end
    def ctl(*args) = control(*args)
    def ctl_now(*); end
    def kill(*)
      return unless @studio
      @studio.record(:kill, synth: @name.to_s, of: @ref) if Thread.current[:oracle_kill] || Node.from_program?
      @studio.end_node_at(self, @studio.sched_time)   # a kill is scheduled like a trigger
    end
    def on_started(&b) = nil
    def live? = true
    def destroyed? = false
    def to_s = "#<Node #{@id} #{@name}>"
  end

  class Bus
    attr_reader :id
    def initialize(id) = (@id = id)
    def to_i = @id
    def to_f = @id.to_f
    def sp_thread_safe? = true
    def free; end
  end

  # The real Node registers /n_go and /n_end handlers with the server; here
  # there is no server and nothing ever sounds.
  class Comms
    def async_add_event_handlers(*); end
    def add_event_handler(*); end
    def rm_event_handler(*); end
    def async_add_event_oneshot(*); end
    def send(*); end
  end

  class Group
    attr_reader :id, :name
    attr_accessor :sub_nodes, :studio
    # An fx block's group holds its fx synth: freeing the group frees the fx.
    attr_accessor :fx
    def initialize(id, name) = (@id, @name = id, name)
    def sp_thread_safe? = true
    def comms = (@comms ||= Comms.new)
    def destroyed? = false
    def on_destroyed(*, &b) = nil
    def on_started(*, &b) = nil
    def kill(*) = @studio&.group_killed(self)
    def to_s = "#<Group #{@id} #{@name}>"
  end

  class Server
    def track_all_notes_off(*); end
    def sched_ahead_time = SonicPi::DEFAULT_SCHED_AHEAD
  end

  # Records what the language asks of the studio, with when it asked.
  class RecordingStudio
    attr_reader :events, :errors
    attr_accessor :lang
    def initialize
      @events = []
      @errors = []
      @mu = Mutex.new
      @ids = 1000
      @buffers = {}
      @buffer_names = {}
      @load_mu = Mutex.new
      @server = Server.new
      @buffer_meta = {}     # buffer number → its file's frames, channels and rate
      @bus_fx = {}          # an fx's input bus → which fx that is
      @pending_fx = 0       # fx started and not yet freed
      @life_mu = Mutex.new
      @life_cv = ConditionVariable.new
      @lives = {}           # node → when it ends, and who is waiting for that
      @timers = []
      Thread.new { reap }
    end
    def pending_fx = @mu.synchronize { @pending_fx }
    def next_id = @mu.synchronize { @ids += 1 }
    # What is heard (a synth, a control, a kill, MIDI out) is timed by when it is heard, counted from the run's beat
    # grid: its logical time, plus its thread's schedule-ahead less the default (a thread on the default is at its own
    # logical time). The runtime records in the same frame (Scheduler#trace_t), so a trace is the same whatever the
    # default. What isn't heard (a cue, a load) stays on the thread's clock.
    HEARD = [:synth, :control, :kill, :midi].freeze
    def record(kind, h)
      # Logical time relative to the job's start; never the wall clock.
      t = @lang.__get_spider_time - @lang.__system_thread_locals.get(:sonic_pi_spider_start_time)
      t += (@lang.current_sched_ahead_time rescue SonicPi::DEFAULT_SCHED_AHEAD).to_r - SonicPi::DEFAULT_SCHED_AHEAD.to_r if HEARD.include?(kind)
      b = (@lang.__get_spider_beat rescue nil)
      rec = { kind: kind, t: t.to_f.round(6), beat: b&.to_f&.round(6), thread: @lang.__thread_path, name: @lang.__current_thread_name.to_s }.merge(h)
      @mu.synchronize { @events << rec }
      rec
    end

    # When something asked for now reaches the engine: the thread's logical
    # time plus its schedule-ahead, or the wall clock plus the default
    # schedule-ahead off a Sonic Pi thread (Server#sched_time).
    def sched_time
      spider = (@lang.__get_spider_time rescue nil)
      return Time.now.to_f + SonicPi::DEFAULT_SCHED_AHEAD unless spider
      spider.to_f + (@lang.current_sched_ahead_time rescue SonicPi::DEFAULT_SCHED_AHEAD).to_f
    end

    # ── What the engine does with a node's lifetime ───────────────────────
    #
    # A synth frees itself when its envelope ends: attack, decay, sustain
    # and release. A sample player's sustain of -1 is the rest of the
    # buffer at its rate (samplers.clj). An fx lives until its group is
    # freed. Sonic Pi's with_fx waits on these, so the oracle must end them
    # when scsynth would, not at once.
    def lifetime(name, args_h, info)
      defaults = (info.arg_defaults rescue {}) || {}
      arg = lambda do |k, d = 0|
        v = args_h[k.to_s]
        v = args_h[k] if v.nil?
        v = defaults[k] if v.nil?
        v.nil? ? d : v.to_f
      end
      attack, decay, release = arg.(:attack), arg.(:decay), arg.(:release)
      meta = @buffer_meta[(args_h["buf"] || args_h[:buf]).to_i] if name.to_s.end_with?("_player")
      # a player's own default sustain is -1, whatever its info says
      sustain = meta && args_h["sustain"].nil? && args_h[:sustain].nil? ? -1 : arg.(:sustain)
      if meta
        rate = arg.(:rate, 1).abs
        return nil if rate.zero?
        length = meta["num_frames"].to_f / meta["sample_rate"]
        length *= (arg.(:finish, 1) - arg.(:start, 0)).abs unless name.to_s.include?("basic_")
        sustain = length / rate - attack - release - decay if sustain == -1
      end
      attack + decay + [sustain, 0].max + release
    end

    def on_node_end(node, &b)
      ended = @life_mu.synchronize do
        life = (@lives[node] ||= { at: nil, done: false, waiting: [] })
        life[:waiting] << b unless life[:done]
        life[:done]
      end
      b.call if ended
    end

    # The node ends at this wall-clock time, unless it ends sooner anyway.
    def end_node_at(node, at)
      @life_mu.synchronize do
        life = (@lives[node] ||= { at: nil, done: false, waiting: [] })
        return if life[:done] || (life[:at] && life[:at] <= at)
        life[:at] = at
        @timers << [at, node]
        @life_cv.signal
      end
    end

    def reap
      loop do
        due = []
        @life_mu.synchronize do
          loop do
            @timers.sort_by!(&:first)
            now = Time.now.to_f
            break if !@timers.empty? && @timers[0][0] <= now
            @timers.empty? ? @life_cv.wait(@life_mu) : @life_cv.wait(@life_mu, @timers[0][0] - now)
          end
          now = Time.now.to_f
          while !@timers.empty? && @timers[0][0] <= now
            at, node = @timers.shift
            life = @lives[node]
            next if life[:done] || life[:at] != at
            life[:done] = true
            due.concat(life[:waiting])
            life[:waiting] = []
          end
        end
        due.each(&:call)
      end
    end

    # The fx's group is freed (with_fx's GC thread, off any Sonic Pi thread):
    # recorded at the wall clock's moment, as logical time from the job's
    # start, and the fx ends when the scheduled free reaches the engine.
    # The wall clock is a few milliseconds late on the time the runtime meant,
    # so check.rb matches a free's time within a tolerance (specs/README.md).
    def group_killed(group)
      fx = group.fx or return
      now = Time.now.to_f
      rec = { kind: :fx_free, t: (now - fx[:start] - SonicPi::DEFAULT_SCHED_AHEAD).round(4), thread: fx[:ref][:thread], name: fx[:name], synth: fx[:ref][:synth], of: fx[:ref] }   # heard now: in the trace's frame (record), less the default
      @mu.synchronize { @events << rec; @pending_fx -= 1 }
      end_node_at(fx[:node], now + SonicPi::DEFAULT_SCHED_AHEAD)
    end
    # A synth's args as the program set them: the studio's own busses left
    # out, and the buffers named rather than numbered, as the runtime names
    # them: a sample by its file, and the studio's random stream (the
    # rand_buf a synth's on_start gives it) by the stream's.
    def program_args(args_h)
      args = args_h.transform_keys(&:to_s)
      args.delete_if { |k, _| k.end_with?("_bus") }
      args["buf"] = @buffer_names[args["buf"]] if args.key?("buf")
      args["rand_buf"] = RAND_STREAM if args.key?("rand_buf") && args["rand_buf"] == rand_buf_id
      args
    end
    def server = @server
    def cent_tuning = 0
    def last_cold_swap_completed_at = nil
    def wait_for_reboot_complete(*) = true
    def volume = 1.0
    def mixer_group = (@mixer_group ||= Group.new(0, "mixer"))
    def mixer_bus = (@mixer_bus ||= Bus.new(10))
    def new_synth_group(id = -1) = Group.new(next_id, "Run-#{id}-Synths")
    def new_fx_group(id = -1) = Group.new(next_id, "Run-#{id}-FX")
    def new_fx_bus = Bus.new(next_id)
    def start; end
    def pause(*); end
    RAND_STREAM = "rand-stream.wav"         # native's etc/buffers file, loaded as that buffer
    def rand_buf_id = 0                      # the studio's random-noise buffer; a synth arg the program never chose
    def new_group(pos, parent, name)
      g = Group.new(next_id, name)
      g.studio = self
      g
    end
    # A synth's out_bus says where its sound goes: into an fx (recorded as
    # `fx`, naming that fx's trigger) or, left out, the job's own mix.
    def trigger_synth(synth_name, group, args_h, info, now = false, t_minus_delta = false, pos = :tail)
      id = next_id
      return Node.new(id, synth_name, args_h) if group.equal?(mixer_group)   # the job's mixer, not the program's
      h = { synth: synth_name.to_s, args: program_args(args_h), now: now }
      into = @mu.synchronize { @bus_fx[(args_h["out_bus"] || args_h[:out_bus]).to_i] }
      h[:fx] = into if into
      rec = record(:synth, h)
      node = Node.new(id, synth_name, args_h)
      node.ref = { t: rec[:t], thread: rec[:thread] }
      node.info = info
      node.studio = self
      in_bus = args_h["in_bus"] || args_h[:in_bus]
      if in_bus && group.is_a?(Group) && group.studio
        ref = { t: rec[:t], thread: rec[:thread], synth: synth_name.to_s }
        group.fx = { node: node, ref: ref, name: rec[:name], start: @lang.__system_thread_locals.get(:sonic_pi_spider_start_time).to_f }
        @mu.synchronize { @bus_fx[in_bus.to_i] = ref; @pending_fx += 1 }
      else
        # A player at rate 0 never ends, and neither would the run that waits
        # for it: that one ends at once.
        life = lifetime(synth_name, args_h, info) || 0
        end_node_at(node, sched_time + life)
      end
      node
    end
    def kill_live_synth(*); end
    def sample_loaded?(path) = @load_mu.synchronize { @buffers.key?(File.expand_path(path)) }
    # Like the real studio, one load at a time: a second thread asking for a
    # sample that is being loaded waits and gets the cached buffer.
    def load_sample(path, server = nil)
      path = File.expand_path(path)
      @load_mu.synchronize do
        return [@buffers[path], true] if @buffers[path]
        meta = AudioMeta.read(path)
        buf = SonicPi::SampleBuffer.new(SonicPi::Buffer.new(@server, next_id, meta["num_frames"], meta["num_chans"], meta["sample_rate"]), path)
        @buffers[path] = buf
        @buffer_names[buf.id] = File.basename(path)
        @buffer_meta[buf.id] = meta
        record(:sample_load, path: File.basename(path))
        [buf, false]
      end
    end
    def method_missing(name, *args)
      raise NoMethodError, "oracle: the recording studio has no #{name} (called with #{args.inspect[0, 120]})"
    end
    def respond_to_missing?(*) = false
  end

  # The link, OSC and MIDI APIs, absent: what Sonic Pi's own tests stand in.
  # A stand-in for Ableton Link: one steady timeline whose beat 0 sits at the
  # first instant the runtime asks about (the job's start), so traces are
  # comparable from run to run.
  class LinkAPI
    def initialize
      @bpm = 60.0
      @epoch = nil
      @mu = Mutex.new
    end
    def epoch_for(t) = @mu.synchronize { @epoch ||= t }
    def link_tempo(*) = @bpm
    def link_is_playing?(*) = false
    def link_transport_state(*) = { playing: false, anchored: false }
    def link_set_bpm!(bpm, *) = (@bpm = bpm.to_f)
    def link_sleep(t, *)
      Kernel.sleep t if t.positive?
    end
    def link_get_beat_at_clock_time(t, *, **) = (t - epoch_for(t)) * @bpm / 60.0
    def link_get_clock_time_at_beat(b, *, **) = epoch_for(Time.now.to_f) + b * 60.0 / @bpm
    def link_get_next_beat_and_clock_time_at_phase(phase, quantum, safety_t, *, **)
      b = link_get_beat_at_clock_time(Time.now.to_f + safety_t)
      nb = (b / quantum).floor * quantum + phase
      nb += quantum while nb < b
      [nb, link_get_clock_time_at_beat(nb)]
    end
    def link_audio_input_set!(*); end
    def link_audio_input_remove!(*); end
    def link_audio_inputs_clear!(*); end
  end
  class OscAPI
    def send_osc_at(*); end
    def osc_flush!(*); end
    def start_stop_cue_server!(*); end
    def cue_server_internal!(*); end
    def set_global_timewarp!(*); end
  end
  # MIDI out is recorded as it is sent: the message's path and its args.
  class MidiAPI
    def initialize(studio) = (@studio = studio)
    def midi_send_at(_t, path, *args) = @studio.record(:midi, path: path, args: args)
    def midi_flush!(*); end
    def midi_system_start!(*); end
    def midi_system_stop!(*); end
    def set_global_timewarp!(*); end
  end

  class Lang
    attr_accessor :mod_sound_studio, :sample_loader, :msg_queue, :event_history
    include SonicPi::RuntimeMethods
    include SonicPi::Lang::Core
    include SonicPi::Lang::WesternTheory
    include SonicPi::Lang::Sound
    include SonicPi::Lang::Midi

    def initialize(studio)
      @mod_sound_studio = studio
      studio.lang = self
      @msg_queue = Queue.new
      __set_default_user_thread_locals!
      @system_init_thread_id = SonicPi::ThreadId.new(-1)
      @settings = SonicPi::Config::Settings.new("/nonexistent/settings.txt")
      @version = SonicPi::Version.new(0, 0, 0, "oracle")
      @server_version = SonicPi::Version.new(1, 0, 0, "final")
      @life_hooks = SonicPi::LifeCycleHooks.new
      @cue_events = SonicPi::IncomingEvents.new
      @job_counter = SonicPi::Counter.new(-1)
      @job_subthreads = {}
      @job_main_threads = {}
      @named_subthreads = {}
      @job_subthread_mutex = Mutex.new
      @osc_cue_server_mutex = Mutex.new
      @user_jobs = SonicPi::Jobs.new
      @session_id = "oracle"
      @snippets = {}
      @system_state = SonicPi::EventHistory.new
      @user_state = SonicPi::EventHistory.new
      @event_history = SonicPi::EventHistory.new
      @gui_cue_log_idxs = SonicPi::Counter.new
      @gui_heartbeats = {}
      @gui_last_heartbeat = nil
      @sample_loader = SonicPi::SampleLoader.new(SAMPLES_DIR)
      @link_api = LinkAPI.new
      @osc_api = OscAPI.new
      @midi_api = MidiAPI.new(studio)
      @system_cue_stamper = SonicPi::CueStamper.new
      @user_methods = Module.new            # define/defonce live here
      singleton_class.include(@user_methods)
      @live_loop_scope_slots = {}
      @live_loop_scope_slots_mutex = Mutex.new
      @block_end_lines = {}
      @block_end_lines_mutex = Mutex.new
      @save_queue = SizedQueue.new(20)
      @gitsave = nil
      @ports = {}
      # Lang::Sound's own state (its wrapped initialize is bypassed by ours)
      @server_init_args = []
      @mod_sound_home_dir = Dir.home
      @simple_sampler_args = [:amp, :amp_slide, :amp_slide_shape, :amp_slide_curve, :pan, :pan_slide, :pan_slide_shape, :pan_slide_curve, :cutoff, :cutoff_slide, :cutoff_slide_shape, :cutoff_slide_curve, :lpf, :lpf_slide, :lpf_slide_shape, :lpf_slide_curve, :hpf, :hpf_slide, :hpf_slide_shape, :hpf_slide_curve, :rate, :slide, :beat_stretch, :rpitch, :attack, :decay, :sustain, :release, :attack_level, :decay_level, :sustain_level, :env_curve]
      @sample_paths_cache = {}
      @job_groups = {}
      @job_group_mutex = Mutex.new
      @job_mixers = {}
      @job_mixers_mutex = Mutex.new
      @job_busses = {}
      @job_busses_mutex = Mutex.new
      @cold_swap_generation = Concurrent::AtomicFixnum.new(0)
      @buffer_lookup_w_hash_syntax = SonicPi::Lang::Sound::BufferLookup.new(lambda { |*a| raise "oracle: buffers are not supported" })
      @life_hooks.on_exit do |job_id, payload|
        @job_mixers_mutex.synchronize { @job_mixers.delete(job_id) }
        kill_job_group(job_id)
        free_job_bus(job_id)
      end
      # The default sched-ahead time lives in the system time state.
      @system_state.sched_ahead_time = SonicPi::DEFAULT_SCHED_AHEAD
      @register_cue_event_lambda = lambda do |t, p, i, d, b, m, address, args, sched_ahead_time = 0|
        address, _sym = *address if address.is_a?(Array)
        @event_history.set(t.to_r, p, i, d, b, m, address.to_s.freeze, args.__sp_make_thread_safe)
        @cue_events.async_event("/spider_thread_sync/#{address}", { time: t.to_r, cue_splat_map_or_arr: args, cue: address })
      end
      init_tuning
    end

    # Sonic Pi identifies a thread by its spawn path: the run is "0", the
    # program's body runs as its first thread "0.0", the threads that spawns
    # are "0.0.0", "0.0.1", ..., theirs "0.0.1.0" and so on. It is the same
    # from run to run, unlike a Ruby thread's identity.
    def __thread_path
      id = __current_thread_id
      id ? id.ids.join(".") : ""
    end

    # A spec's horizon (its `# horizon: N` line): a thread whose sleep takes
    # it past N seconds of logical time stops there. It lets a program that
    # never ends, like a live_loop, be recorded; the runtime applies the
    # same rule, so both stop at the same place.
    attr_accessor :horizon

    def sleep(beats)
      res = super
      if @horizon && (__get_spider_time - __system_thread_locals.get(:sonic_pi_spider_start_time)) > @horizon
        raise SonicPi::Stop
      end
      res
    end

    def control(*args)
      Thread.current[:oracle_control] = true
      super
    ensure
      Thread.current[:oracle_control] = false
    end

    def kill(node)
      Thread.current[:oracle_kill] = true
      super
    ensure
      Thread.current[:oracle_kill] = false
    end

    # The runtime turns an exception into prose for the GUI; keep the raw
    # class, message and line so a trace can pin them exactly.
    def __error(e, m = nil)
      line = __extract_linenum_of_error(e)
      # an error from before the program ran (the preparser's) has no line of the program's: native's own is not one
      line = -1 unless (e.backtrace || []).any? { |l| l.start_with?("oracle:") }
      message = e.message.lines.first.to_s.strip
      # Ruby's wording for a missing method or name is the interpreter's, not
      # the language's: keep the method or name and drop the receiver and the
      # quoting, so a runtime on another interpreter can say the same thing.
      if e.is_a?(NameError)
        cut = message.index(" for ")
        message = message[0, cut] if cut
        message = message.tr("`", "'")
      end
      @mod_sound_studio.errors << { class: e.class.name, message: message, line: line, thread: __thread_path, name: __current_thread_name.to_s }
      super
    end

    # Messages to the user carry only the thread's name; tag each with the
    # thread's path too, so output and log can say which thread spoke.
    SEQ = Concurrent::AtomicFixnum.new(0)
    def __enqueue_multi_message(m_type, m)
      raise "Can only use __enqueue_multi_message in a job thread" unless __current_job_id
      # Batches reach the queue on timers, so arrival order is not emission
      # order; a sequence taken now is.
      __system_thread_locals.get(:sonic_pi_local_spider_delayed_messages) << [m_type, m, __thread_path, SEQ.increment]
    end
  end

  def self.run(code, timeout: 60)
    studio = RecordingStudio.new
    lang = Lang.new(studio)
    horizon = nil
    code.each_line do |l|
      break unless l.start_with?("#")
      horizon = l[10..].to_f if l.start_with?("# horizon:")
    end
    lang.horizon = horizon
    lang.__spider_eval(code, { workspace: "oracle" })
    # The runtime announces :all_jobs_completed once the job's main thread and
    # every subthread it spawned have ended and the last synth has been heard.
    # With a horizon, a thread left waiting on a sync that will never come
    # would keep that from happening, so past the horizon (plus the time the
    # runtime takes to post its last messages) the run is stopped instead.
    deadline = Time.now + timeout
    horizon_deadline = horizon && Time.now + horizon + 3
    messages = []
    loop do
      begin
        m = lang.msg_queue.pop(true)
        messages << m
        break if m[:type] == :all_jobs_completed
        # a run that never started (its program refused before it ran: the preparser) is only ever "completed":
        # native posts that after all_jobs_completed otherwise, so this is the aborted run's end
        break if m[:type] == :job && m[:action] == :completed
      rescue ThreadError
        if horizon_deadline && Time.now > horizon_deadline
          begin
            lang.__stop_jobs
          rescue StandardError
            nil   # the stand-in OSC and MIDI APIs have no flush; the jobs are stopped by then
          end
          break
        end
        raise "oracle: timed out after #{timeout}s" if Time.now > deadline
        sleep 0.01
      end
    end
    # An fx is freed in the background after its run completes: its block,
    # the threads the block started and its synths have ended, then its
    # kill_delay. Wait for that (a horizon's run was stopped, and what is
    # freed past the horizon is not recorded anyway).
    fx_deadline = Time.now + (horizon ? 0 : timeout)
    sleep 0.01 while studio.pending_fx > 0 && Time.now < fx_deadline
    # A thread's last messages are posted on a timer, sched-ahead after its
    # final logical instant; give the slowest of them time to land.
    sleep 1.0
    messages << lang.msg_queue.pop(true) until lang.msg_queue.empty?
    trace(studio, messages, horizon)
  end

  # Within one thread, order is the program's own and is kept. Across
  # threads at one instant there is no defined order, so the thread's spawn
  # path decides, which makes a trace the same from run to run.
  # An fx's free comes after what its thread did at that instant.
  def self.ordered(items)
    items.each_with_index.sort_by { |e, i| [e[:t], e[:thread].to_s, e[:kind] == :fx_free ? 1 : 0, e[:seq] || i] }.map { |e, _| e.reject { |k, _| k == :seq } }
  end

  def self.trace(studio, messages, horizon = nil)
    events = ordered(studio.events.reject { |e| horizon && e[:kind] == :fx_free && e[:t] > horizon })
    errors = studio.errors.sort_by { |e| [e[:line].to_i, e[:thread], e[:class]] }
    syntax = messages.select { |m| m[:type] == :syntax_error }.map { |m| { class: "SyntaxError", message: m[:val].to_s.lines.first.to_s.strip, line: m[:line], thread: "0.0", name: "" } }
    output = []   # what `puts`/`print` showed the user, in logical time
    log = []      # everything else the runtime told the user
    messages.each do |m|
      next unless m[:type] == :multi_message
      m[:val].each do |style, text, path, seq|
        text = text.to_s.gsub(SAMPLES_DIR, "<samples>").gsub(SAMPLES_DIR.sub(Dir.home, "~"), "<samples>")   # never a machine path
        next if text.start_with?("Timing warning", "Timing error")             # wall-clock artefacts of running in real time
        entry = { t: m[:runtime].to_f.round(6), thread: path.to_s, name: m[:thread_name].to_s, text: text, seq: seq }
        (style == 1 ? output : log) << entry
      end
    end
    { events: events, errors: errors + syntax, output: ordered(output), log: ordered(log) }
  end
end

if __FILE__ == $0
  code = File.read(ARGV[0])
  puts JSON.pretty_generate(Oracle.run(code))
end
