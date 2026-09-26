# SPDX-License-Identifier: AGPL-3.0-or-later
# Sonic Pi's threads without threads: every thread is a fiber with its own
# logical clock, and one scheduler runs whichever is due next. Nothing waits
# on a wall clock; a sleep is a jump in logical time, so a whole program runs
# in an instant and its trace is the same every time.
#
# What is kept faithful to Sonic Pi:
#   * a thread's identity is its spawn path (the run is [0], the program's
#     body [0, 0], its children [0, 0, n] in spawn order);
#   * a spawned thread runs at once, until it first sleeps, syncs or ends,
#     and only then does the parent carry on (CRuby's threads do this, and
#     programs can see it);
#   * threads due at the same instant run in the order they became due;
#   * cues and syncs order by (time, priority, thread path, delta), so a
#     sync sees a cue at its own instant only when the cue sorts after it.
module SonicPi
  class << self
    attr_accessor :current_process
    # Link's timeline, the session's: beat 0 sounds the default schedule-ahead
    # after the first run's start (link_origin), until the tempo changes; then
    # it runs on from link_anchor, [the session's time the tempo last changed
    # (as it sounds), the beat then].
    attr_accessor :link_origin, :link_anchor
    # The session's schedule-ahead (set_sched_ahead_time!): every thread's, but one that has set its own
    # (use_sched_ahead_time, with_sched_ahead_time, use_real_time). A plain global: set, it holds from then on, for
    # the threads already running as for the runs to come.
    attr_writer :sched_ahead
    def sched_ahead = @sched_ahead || DEFAULT_SCHED_AHEAD
  end

  # One Sonic Pi thread.
  class Process
    attr_accessor :path, :name, :fiber, :time, :beat, :bpm, :density, :locals, :rand,
                  :spawned, :delta, :priority, :slept, :synced, :in_time_warp, :counters,
                  :last_sync, :wake_time, :seq, :done, :own_sched_ahead, :start, :file,
                  :state_cache, :wait_line, :time_warp_start,
                  :uid, :parent_uid, :state, :ended_at, :active_at, :events, :redefs, :errored,
                  :parent, :members, :fx, :nodes, :body_end, :subtree_at,
                  :reset_to,
                  :spawn_line,  # the line it was started from: its own when its fiber holds no frame of the program's (a sample's loading thread)
                  :group        # the group its run belongs to (a card's, a buffer's): a thread is born into its parent's, and a live loop follows the run that redefines it
    # Where the thread is on the wall clock, on the session's clock (its job's
    # start plus logical time, so threads of different runs compare; a trace's
    # one job starts at 0). Sonic Pi's sleep
    # wakes 0.2s before the time it sleeps to, and does not sleep at all when
    # that would be under 0.2s from now (Lang::Core#sleep); a sync wakes when
    # the cue is made. Only with_fx sees it: an fx is freed on the wall clock.
    attr_accessor :wall

    def initialize(path, name, parent)
      @path = path
      @name = name
      @parent = parent
      @wall = parent ? parent.wall : 0.0
      @fx = parent && parent.fx     # the with_fx the thread is in (an FxFrame)
      @members = []                 # the threads it started: a node's members, as a block's are
      @nodes = []                   # the sounds it made: a thread is done once they end
      if parent
        @time = parent.time
        @beat = parent.beat
        @bpm = parent.bpm
        @density = parent.density
        @locals = parent.locals.dup     # settings are values, never shared structure
        @priority = parent.priority
        @own_sched_ahead = parent.own_sched_ahead   # a use_sched_ahead_time is inherited; the session's is read live
        @start = parent.start
        @file = parent.file
      else
        @time = 0.0
        @beat = 0.0
        @bpm = :link
        @density = 1.0
        @locals = {}
        @priority = 0
        @start = 0.0        # the job's start on the host clock (RT); a trace starts at 0
        @file = "run"
      end
      @group = parent ? parent.group : 0
      @spawned = 0
      @delta = 0
      @slept = false
      @synced = false
      @in_time_warp = false
      @counters = {}
      @done = false
      @state_cache = []     # what this thread set since it last slept: a get sees it at once
      @state = :running
      @events = 0
      @redefs = 0
    end

    def id = (@id ||= @path.join("."))      # a thread's path never changes
    # its own schedule-ahead when it has set one (nil when it has not), else the session's
    def sched_ahead = @own_sched_ahead || SonicPi.sched_ahead
    def sched_ahead=(t)
      @own_sched_ahead = t
    end
    def bpm_value = (@bpm == :link ? SonicPi.link_bpm : @bpm) * @density
    def sleep_mul = 60.0 / bpm_value

    # Logical time and beat after a sleep of beat_delta beats. On the Link
    # timeline (the default, a steady 60 bpm in the oracle) time is the
    # beat's clock time less the sched-ahead, measured from a start that
    # was itself computed with the default sched-ahead (DEFAULT_SCHED_AHEAD); on a numeric
    # tempo it moves by beats times the beat length.
    def advance!(beat_delta)
      new_beat = @beat + (beat_delta / @density)
      if @bpm == :link
        @time = link_time_at_beat(new_beat)
      else
        @time = @time + beat_delta * sleep_mul
      end
      @beat = new_beat.to_f
    end

    # This job's logical time for a beat on Link's timeline, and the beat for a
    # time. The timeline is the session's (SonicPi.link_origin, link_anchor),
    # so every run keeps to one beat grid, as Link's does. The first run of a
    # session, and a trace's one run, keep the plain arithmetic.
    def link_time_at_beat(beat)
      link = SonicPi.link_bpm
      anchor = SonicPi.link_anchor
      origin = SonicPi.link_origin || @start
      if anchor
        (anchor[0] - @start + (beat - anchor[1]) * 60.0 / link - sched_ahead).to_f
      elsif @start == origin
        ((link == 60.0 ? beat : beat * 60.0 / link) - sched_ahead + DEFAULT_SCHED_AHEAD).to_f
      else
        (origin - @start + beat * 60.0 / link - sched_ahead + DEFAULT_SCHED_AHEAD).to_f
      end
    end

    def link_beat_at_time(time)
      link = SonicPi.link_bpm
      anchor = SonicPi.link_anchor
      heard = @start + time + sched_ahead
      return anchor[1] + (heard - anchor[0]) * link / 60.0 if anchor
      (heard - (SonicPi.link_origin || @start) - DEFAULT_SCHED_AHEAD) * link / 60.0
    end

    # On the plain arithmetic: the session's first run, before any tempo change.
    def link_plain? = SonicPi.link_anchor.nil? && @start == (SonicPi.link_origin || @start)

    # Ordered on the session's clock: a job's logical time from its start, so a
    # cue from one run and a sync in another compare as Sonic Pi's wall-clock
    # spider times do. A trace's one job starts at 0, so nothing changes there.
    def key(priority = @priority) = [@start + @time, priority, @path, @delta]
  end

  # A cue that happened, or the point a sync waits from.
  class CueEvent
    attr_reader :time, :priority, :path, :delta, :beat, :bpm, :address
    attr_accessor :wall     # the cuer's wall clock when it cued (see Process#wall)
    # the value as bytes (time_state.rb), and as frozen values once asked for: nothing can change what was stored
    def val = (@val_decoded ||= [TimeState.decode(@bytes)])[0]

    def initialize(time, priority, path, delta, beat, bpm, address, val)
      @time = time
      @priority = priority
      @path = path
      @delta = delta
      @beat = beat
      @bpm = bpm
      @address = address
      @bytes = TimeState.encode(val)
    end

    def key = [@time, @priority, @path, @delta]

    def self.compare(a, b)
      return a[0] <=> b[0] unless a[0] == b[0]
      return a[1] <=> b[1] unless a[1] == b[1]
      c = compare_paths(a[2], b[2])
      return c unless c == 0
      a[3] <=> b[3]
    end

    def self.compare_paths(a, b)
      a.each_with_index do |el, idx|
        return 1 if idx >= b.size
        return -1 if el < b[idx]
        return 1 if el > b[idx]
      end
      b.size > a.size ? -1 : 0
    end
  end

  # One with_fx block. Sonic Pi frees its fx (with_fx's GC thread) once the
  # block has ended, every thread the block's own thread started in it has
  # ended along with all of theirs (a thread is done only when its own sounds
  # have ended), and every sound the block's own thread made in it has ended
  # (a nested fx ends when it is freed); then it waits kill_delay. All of
  # that is on the wall clock (Process#wall), and the free reaches the engine
  # the default schedule-ahead later, which is when the fx node ends.
  class FxFrame
    # A with_fx block is a node of the tree like a thread is: what it started are its members, what it played
    # its nodes, and the end of its own code its body_end, so the one subtree walk (Scheduler#subtree_end)
    # decides when it is over, as it does for a thread and a group.
    attr_accessor :owner, :parent, :node, :ref, :kill_delay, :free_at,
                  :members, :nodes, :subtree_at, :bus, :group, :synths
    # when a live loop last moved out: the fx waited for it until then; and, live, the engine time before
    # which its group must not be freed (the handover of the loop's own fx out of it is still under way)
    attr_accessor :released_at, :free_not_before
    # a loop's own fx after a hand-over (move_loop): the node id the page first
    # knew it by, which its fx_free names
    attr_accessor :first_node
    # RT, for the threads view: its row, its line, when it opened (engine clock)
    attr_accessor :uid, :line, :opened_at

    def initialize(owner, parent)
      @owner = owner
      @parent = parent
      @kill_delay = 1
      @members = []       # threads the block started (or live loops moved into it)
      @nodes = []         # sounds the block's own thread made
    end
    attr_accessor :block_end
    alias body_end block_end   # the walk's name for the end of the block's own code

    def node_end = @free_at && @free_at + @owner.sched_ahead   # heard a schedule-ahead after, its thread's
  end

  # What a sound needs from the engine, numbered here so the sound's OSC can
  # name it the moment it is made: each synthdef by index, each sample file
  # by buffer number. The numbers last as long as the engine does (a live
  # session booting again keeps them), and the host is told a name the first
  # time it is numbered, and loads it. A host that loads a sample before a
  # program asks for it numbers it with claim_buffer, and needs no telling.
  module EngineIds
    FIRST_BUFFER = 100
    @synthdefs = {}
    @buffers = {}
    @next_buffer = FIRST_BUFFER

    def self.synthdef(name)
      @synthdefs[name] || begin
        i = @synthdefs[name] = @synthdefs.size
        Native.host("/sonic-pi/synthdef", i, name)
        i
      end
    end

    def self.buffer(file)
      @buffers[file] || begin
        n = claim_buffer(file)
        Native.host("/sonic-pi/sample", n, file)
        n
      end
    end

    def self.claim_buffer(file) = (@buffers[file] ||= (@next_buffer += 1) - 1)
    def self.free_buffer(file) = @buffers.delete(file)
  end

  class Scheduler
    attr_reader :events, :errors, :output, :log, :current

    def initialize
      @queue = []          # processes due to run: [wake_time, seq, process]
      @seq = 0
      @history = CueHistory.new   # every cue, filed under its address (cue_history.rb)
      @waiters = []        # [process, key, addresses]
      @named = {}          # live named threads
      @scope_slots = {}    # live_loop name → scope number
      @events = []
      @errors = []
      @output = []
      @log = []
      @current = nil
      @cent_tuning = 0
      @time_warp = 0.0
      @volume = 1.0
      @drive = 1.0
      @node_id = 0
      @uid = 0
      @procs = {}           # RT: every thread by uid, for the process table (finished ones linger)
      @fx_open = []         # with_fx frames not yet freed
      @fx_pending = []      # of those, the ones whose block has ended
      @fx_frees = []        # RT: [engine time, frame] frees not sent yet
      @node_frees = []      # RT: [engine time, node id] sounds a group's stop fades, freed at the end of the fade
      @group_parent = {}    # group → the group it sits in (group_under): a stop reaches a group and every group under it
      @group_uid = {}       # group → its uid in the process table, where it is a row of its own (KIND_GROUP), the runs hanging from it
      @busses = []          # RT: [bus, free from] busses to use again
      @next_bus = BUS_FIRST
      @fx_gone = []         # RT: fx freed a moment ago, still shown
      @sounds = []          # RT: [uid, thread, frame, node, line, start] sounds shown
    end

    # ── Running ───────────────────────────────────────────────────────────
    #
    # Two modes over one queue. NRT (run): resume the next due thread at
    # once; logical time is only a number, and a program that never ends is
    # traced up to max_time. RT (start_job + step): a thread is resumed when
    # the host's clock reaches its wake time less its schedule-ahead, and
    # step says when to come back. The scheduler never sleeps itself; the
    # host owns the one real wait.

    # A trace has a horizon: a program that never ends (a live_loop with no
    # stop) is traced up to max_time of logical time, and a thread that
    # spins without sleeping is cut off by the event cap. Both are recorded
    # as a runtime error so the trace says it was cut short.
    attr_accessor :max_time, :max_events, :lang, :cent_tuning, :volume, :drive
    # The global time warp, in seconds: every message to the engine, and every
    # record's time, that much later (earlier when negative), as Sonic Pi's
    # set_global_timewarp! shifts every sound, OSC and MIDI message.
    attr_accessor :time_warp
    attr_reader :live

    # RT: every record leaves as it is made, as OSC for the page (sp_host.c's
    # GUI stream), and every sound as the OSC the engine plays (its audio
    # stream), each saying which line of the program it came from.
    def live=(on)
      @live = on && SonicPi.const_defined?(:Native) && SonicPi::Native.respond_to?(:gui)
      @line_lookup = @live && SonicPi::Native.respond_to?(:line)
    end
    # A spec's horizon (its `# horizon: N` line): a thread whose sleep takes
    # it past N seconds of logical time stops there, as the oracle's does.
    attr_accessor :stop_after

    # NRT: runs one job to its end (or the horizon). The run itself is
    # thread [0]; the program is its first child, spawned with seed 0, the
    # way Sonic Pi's spider does it.
    def run(lang, tables, file = "run", &program)
      @lang = lang
      @tables = tables
      @cut_short = nil
      start_job_process(0, 0.0, file, &program)
      until @queue.empty? || @cut_short
        @queue.sort! { |a, b| a[0] == b[0] ? a[1] <=> b[1] : a[0] <=> b[0] }
        break if @max_time && @queue[0][0] > @max_time
        _, _, p = @queue.shift
        resume(lang, p, nil)
      end
      # Anyone still waiting on a sync that never came is abandoned, as in
      # Sonic Pi a run like that never completes.
      @errors << { class: "SonicPi::TraceCutShort", message: @cut_short, line: -1, thread: "0", name: "" } if @cut_short
    end

    # RT: starts a job at the host's time now. Its head runs at once, up to
    # its first sleep, as a Run does in Sonic Pi. Returns the job's id.
    def start_job(lang, tables, now, file, group = 0, &program)
      @lang = lang
      @tables = tables
      @next_job ||= 0
      id = @next_job
      @next_job += 1
      start_job_process(id, now, file, group, &program)
      id
    end

    def start_job_process(id, start, file, group = 0, &program)
      job = Process.new([id], nil, nil)
      job.group = group
      register(job, group_uid(group) || -1)   # a run hangs from its group in the table
      job.rand = SonicPi::Rand::State.new(@tables, 0, 0, nil, 0)
      job.locals = default_locals
      job.start = start
      job.wall = start
      job.file = file
      # Link's timeline is the session's: its first run starts on beat 0, and a
      # later run at the beat Link has when it starts, as a Run in Sonic Pi
      # takes Link's beat; so a sync across runs keeps both on one beat grid.
      SonicPi.link_origin ||= start
      job.beat = job.link_beat_at_time(0.0) unless job.link_plain?
      @current = job
      SonicPi.current_process = job
      spawn(@lang, { seed: 0 }, &program)
    end

    # RT: runs every thread whose wake time the host's clock has reached, and
    # a record it makes sounds its schedule-ahead after that (Sonic Pi's
    # spider time plus sched_ahead_time): so a run's first beat leaves with
    # the same headroom as every other. Returns when the host should call
    # again (on that clock), or nil when nothing is waiting.
    def step(now)
      @now = now
      loop do
        break if @queue.empty?
        # by when each is due on the session's clock: a wake time is its own
        # run's, and runs start at different moments
        @queue.sort! do |a, b|
          ta = a[2].start + a[0]
          tb = b[2].start + b[0]
          ta == tb ? a[1] <=> b[1] : ta <=> tb
        end
        wake, _, p = @queue[0]
        break if p.start + wake > now
        @queue.shift
        resume(@lang, p, nil)
      end
      send_fx_frees(now)
      prune_history!
      nexts = @queue.map { |(wake, _, p)| p.start + wake } + @fx_frees.map { |(at, _)| at - FREE_LEAD } + @node_frees.map { |(at, _)| at - FREE_LEAD }
      nexts.min
    end

    # Link's tempo changes at `at` (the session's clock, as it sounds): every
    # job's beat carries on from where it is then, at the new tempo, as Link's
    # timeline does (SuperSonic's ClockworkClock#setBpm keeps the beat at the
    # moment of change), and a thread sleeping on the timeline wakes at its
    # beat's new time, as Sonic Pi's link_sleep has it.
    def set_link_bpm(bpm, at)
      old = SonicPi.link_bpm
      if SonicPi.link_origin || SonicPi.link_anchor
        anchor = SonicPi.link_anchor
        beat = anchor ? anchor[1] + (at - anchor[0]) * old / 60.0 : (at - SonicPi.link_origin - DEFAULT_SCHED_AHEAD) * old / 60.0
        SonicPi.link_anchor = [at, beat]
      end
      SonicPi.link_bpm = bpm.to_f
      return nil unless SonicPi.link_anchor
      @queue.each do |entry|
        q = entry[2]
        next unless q.bpm == :link && !q.in_time_warp && q.state == :sleeping
        next if q.beat < SonicPi.link_anchor[1]                  # due before the change: it sounds as it was
        entry[0] = q.wake_time = q.time = q.link_time_at_beat(q.beat)
      end
      nil
    end

    # RT: the host lost `seconds` (a dialog held the page, the audio was suspended, the clock jumped):
    # everything still to come moves that much later, so the music carries on from where it was rather
    # than racing to catch up. Every process's start, the fx frees waiting, and Link's origin and anchor.
    def hold(seconds)
      return if seconds <= 0
      @procs.each_value { |p| p.start += seconds }
      @fx_frees.each { |entry| entry[0] += seconds }
      @node_frees.each { |entry| entry[0] += seconds }
      SonicPi.link_origin += seconds if SonicPi.link_origin
      SonicPi.link_anchor = [SonicPi.link_anchor[0] + seconds, SonicPi.link_anchor[1]] if SonicPi.link_anchor
      nil
    end

    # RT: every job stops where it stands; nothing is resumed again.
    def stop_all
      @procs.each_value { |p| __stopped(p) }
      @queue.clear
      @waiters.clear
      @named.clear
      @scope_slots.clear
      @pending = {}
      @fx_open.clear            # the page frees every node (Bridge#silence), groups and fx with them
      @fx_pending.clear
      @fx_frees.clear
      @node_frees.clear
      @busses.clear
      @next_bus = BUS_FIRST
      @fx_gone.clear
      @sounds.clear
      @studio = false           # the page freed the studio with everything else
      SonicPi.live_audio_nodes&.clear   # live_audio's synths went too: the next call starts one afresh
    end

    # RT: one job stops where it stands; the others carry on.
    def stop_job(id)
      @procs.each_value { |p| __stopped(p) if p.path[0] == id }
      @queue.reject! { |(_, _, p)| p.path[0] == id }
      @waiters.reject! { |(p, _, _)| p.path[0] == id }
      @named.delete_if { |_, p| p.path[0] == id }
      # its fx go now, with whatever is still sounding in them
      gone = @fx_open.select { |f| f.owner.path[0] == id }
      @fx_open -= gone
      @fx_pending -= gone
      sent, @fx_frees = @fx_frees.partition { |(_, f)| f.owner.path[0] == id }
      (gone + sent.map { |(_, f)| f }).each { |f| free_fx_now(f) }
      settle_fx
      nil
    end

    # RT: a group stops — every thread born into it or moved into it, wherever its run began. What is sounding
    # goes gently: its fx and its bare sounds are turned down over `fade` seconds and freed as the fade ends (at
    # once when fade is 0). A group is the unit the GUI plays and stops by: a card's runs are one group, a
    # buffer's runs another, so Stop on a card reaches a loop its second run redefined and nothing else.
    GROUP_FREE_GRACE = 0.05
    # Groups nest: a group under another goes when that one is stopped (a card's under the cards', a buffer's
    # under the buffers', a track's buffers under the track).
    def group_under(group, parent)
      @group_parent[group] = parent
      group_uid(parent)
      group_uid(group)
    end

    # A group's uid: given the first time the group is seen (a run in it, or nesting), so the table can show it
    def group_uid(group)
      return nil if group.nil? || group == 0
      @group_uid[group] ||= (@uid += 1)
    end

    def within?(group, ancestor)
      g = group
      while g
        return true if g == ancestor
        g = @group_parent[g]
      end
      false
    end

    def stop_group(group, fade, now) = stop_where(->(p) { within?(p.group, group) }, fade, now)

    # A subtree stops: the thread with this uid (a run's, a live loop's, an in_thread's) and every thread under it,
    # or an fx block's (its uid): the threads started inside it and its sounds. Every block that spawns or scopes
    # is a node of the one tree, so any of them can be stopped as one, with the same fade.
    def stop_subtree(uid, fade, now)
      frame = (@fx_open + @fx_pending).find { |f| f.uid == uid }
      if frame
        roots = frame.members
        stop_where(->(p) { roots.any? { |r| descends?(p, r) } }, fade, now, [frame])
      else
        root = @procs[uid] or return nil
        stop_where(->(p) { descends?(p, root) }, fade, now)
      end
    end

    def descends?(p, root)
      q = p
      while q
        return true if q.equal?(root)
        q = q.parent
      end
      false
    end

    # The stop itself, over the threads `of` picks: they stop now; their fx (and any `frames` given) and their bare
    # sounds are turned down over `fade` and freed at its end, or freed at once when there is no fade.
    def stop_where(of, fade, now, frames_too = [])
      mine = @procs.each_value.select(&of)
      mine.each { |p| __stopped(p) }
      @queue.reject! { |(_, _, p)| of.call(p) }
      @waiters.reject! { |(p, _, _)| of.call(p) }
      @named.delete_if { |_, p| of.call(p) }
      gone = @fx_open.select { |f| of.call(f.owner) || frames_too.include?(f) }
      @fx_open -= gone
      @fx_pending -= gone
      sent, @fx_frees = @fx_frees.partition { |(_, f)| of.call(f.owner) || frames_too.include?(f) }
      frames = gone + sent.map { |(_, f)| f }
      inside = ->(p, frame) { of.call(p) || (frame && frames.include?(frame)) }
      if fade > 0 && @live
        at = now + fade + GROUP_FREE_GRACE
        frames.each do |f|
          next unless f.group
          Native.n_set(0.0, -1, NODE_BASE + f.node.id, { "amp" => 0.0, "amp_slide" => fade }) if f.node
          @fx_frees << [at, f]
        end
        # a sound outside any fx of its own plays into the studio: turned down itself, and freed after
        @sounds.each do |(_, p, frame, node, _, _)|
          next unless inside.call(p, frame) && node.ends_at > now
          node.ends_at = at
          next if frame && frames.include?(frame)   # inside one of the group's fx: it goes with the fx
          Native.n_set(0.0, -1, NODE_BASE + node.id, { "amp" => 0.0, "amp_slide" => fade })
          @node_frees << [at, node.id]
        end
      else
        frames.each { |f| free_fx_now(f) }
        @sounds.each { |(_, p, frame, node, _, _)| node.ends_at = now if inside.call(p, frame) && node.ends_at > now }
      end
      settle_fx
      nil
    end

    # The thread and all it started join a group (a live loop redefined from another run's, and its members).
    def regroup(p, group)
      p.group = group
      p.members.each { |c| regroup(c, group) }
    end

    def __stopped(p)
      return if p.ended_at || p.path.size == 1
      p.state = :stopped
      p.ended_at = p.start + p.time
      p.body_end ||= p.wall
    end

    # ── The process table ─────────────────────────────────────────────────
    #
    # RT: every thread as a row of numbers, for the page to read straight out
    # of the runtime's memory (sp_process_table): no JSON, no copies of
    # strings. Names travel once per thread, in the GUI stream. A finished thread
    # stays a moment so the page can show it ending.
    PROCESS_STATES = { running: 0, sleeping: 1, waiting: 2, done: 3, error: 4, stopped: 5 }
    LINGER = 2.5

    # The thread holding a name, asked from a thread at this wall clock. A
    # named thread keeps its name until it is done: its body has ended and
    # so have its own sounds (Sonic Pi takes it off the job only after its
    # tracker), which is on the wall clock too.
    def named_process(name, wall = nil)
      p = @named[name] or return nil
      return p if wall.nil? || holds_name?(p, wall)
      @named.delete(name)
      nil
    end

    def holds_name?(p, wall)
      return true unless p.done
      e = p.body_end
      p.nodes.each do |n|
        x = node_end(n)
        return true if x.nil?
        e = x if x > e
      end
      e > wall
    end

    def process_table(now)
      alive = {}
      ended = {}
      @procs.each_value do |p|
        next if p.path.size == 1
        j = p.path[0]
        if p.ended_at
          ended[j] = p.ended_at if ended[j].nil? || p.ended_at > ended[j]
        else
          alive[j] = true
        end
      end
      keep = {}
      @procs.each_value do |p|
        gone = if p.path.size == 1
                 !alive[p.path[0]] && (ended[p.path[0]].nil? || now - ended[p.path[0]] > LINGER)
               else
                 p.ended_at && now - p.ended_at > LINGER
               end
        keep[p.uid] = true unless gone
      end
      # So do the threads whose with_fx blocks and sounds are still shown.
      @sounds.reject! { |s| now - s[3].ends_at > SOUND_LINGER }
      @sounds.shift(@sounds.size - MAX_SOUNDS) if @sounds.size > MAX_SOUNDS
      @fx_gone.reject! { |f| now - (f.free_at + f.owner.sched_ahead) > LINGER }
      (@fx_open + @fx_gone).each { |f| keep[f.owner.uid] = true if f.uid && @procs[f.owner.uid] }
      @sounds.each { |s| keep[s[1].uid] = true if @procs[s[1].uid] }
      # A thread's ancestors stay while it does, finished or not, so the tree
      # keeps its shape: a run's main thread ends long before its live loops.
      @procs.each_value do |p|
        next unless keep[p.uid]
        q = @procs[p.parent_uid]
        while q && !keep[q.uid]
          keep[q.uid] = true
          q = @procs[q.parent_uid]
        end
      end
      @procs.delete_if { |uid, _| !keep[uid] }
      rows = []
      # The groups first, as the containers the runs hang from: live (0) while anything of theirs is, else over (3).
      live_groups = {}
      @procs.each_value { |p| live_groups[p.group] = true if p.path.size > 1 && !p.ended_at }
      @fx_open.each { |f| live_groups[f.owner.group] = true }
      @sounds.each { |(_, p, _, node, _, _)| live_groups[p.group] = true if node.ends_at > now }
      @group_uid.each do |g, uid|
        going = live_groups.any? { |lg, _| within?(lg, g) }
        parent = @group_parent[g] ? (@group_uid[@group_parent[g]] || -1) : -1
        rows.push(uid, parent, -1, KIND_GROUP, going ? 0 : 3, -1, -1.0, 0.0, 0.0, -1.0, 0, 0, -1.0, -1, g)
      end
      @procs.each_value do |p|
        job = p.path.size == 1
        kind = if job then 0
               elsif p.path.size == 2 then 1
               elsif p.name && p.name.to_s.start_with?("live_loop_") then 2
               elsif p.name then 3
               else 4
               end
        state = job ? (alive[p.path[0]] ? 0 : 3) : PROCESS_STATES[p.state]
        ended_at = job ? (alive[p.path[0]] ? nil : ended[p.path[0]]) : p.ended_at
        wake = p.state == :sleeping && !job ? p.start + p.wake_time : -1.0
        rows.push(p.uid, thread_parent(p), p.path[0], kind, state, p.wait_line || -1, wake, p.beat, p.bpm_value,
                  p.active_at || -1.0, p.events, p.redefs, ended_at || -1.0, -1, p.group)
      end
      # A with_fx block: running (0), its block ended and waiting on its
      # threads and sounds (1), freed (3). node: its fx, whose record names it.
      (@fx_open + @fx_gone).each do |f|
        next unless f.uid && @procs[f.owner.uid] && !scope_fx?(f)
        o = f.owner
        freed = f.free_at && f.free_at + o.sched_ahead
        state = freed ? 3 : f.block_end ? 1 : 0
        rows.push(f.uid, hang_from(f.parent, o), o.path[0], KIND_FX, state, f.line || -1, -1.0, 0.0, 0.0,
                  f.opened_at, f.members.size, 0, freed || -1.0, f.node ? f.node.id : -1, o.group)
      end
      # A sound: sounding (0) or ended (3), from its start to its end.
      @sounds.each do |(uid, p, frame, node, line, start)|
        next unless @procs[p.uid]
        finish = node.ends_at
        kind = node.name.end_with?("_player") ? KIND_SAMPLE : KIND_SYNTH
        rows.push(uid, hang_from(frame, p), p.path[0], kind, now >= finish ? 3 : 0, line || -1, -1.0, 0.0, 0.0,
                  start, 0, 0, finish, node.id, p.group)
      end
      rows
    end

    KIND_FX = 6
    KIND_SYNTH = 7
    KIND_SAMPLE = 8
    KIND_GROUP = 9
    SOUND_LINGER = 1.0
    MAX_SOUNDS = 300

    # Where the threads view hangs what `owner` made inside `frame`: from the
    # innermost with_fx block of owner's own it is in (a live loop's scope fx
    # is not shown), else from owner itself.
    def hang_from(frame, owner)
      f = frame
      while f && f.owner.equal?(owner)
        return f.uid unless scope_fx?(f)
        f = f.parent
      end
      owner.uid
    end

    def scope_fx?(f) = f.node && f.node.name == "sonic-pi-fx_scope_out"

    # A thread hangs from the with_fx block it was started in, or moved into.
    def thread_parent(p)
      return p.parent_uid unless p.parent && p.path.size > 1
      f = p.fx
      f = f.parent while f && f.owner.equal?(p)
      hang_from(f, p.parent)
    end

    def register(p, parent_uid)
      p.uid = (@uid += 1)
      p.parent_uid = parent_uid
      @procs[p.uid] = p if @live
    end

    def idle? = @queue.empty? && @waiters.empty?


    def check_cap!(p)
      return unless @max_events && @events.size >= @max_events && !@cut_short
      @cut_short = "trace stopped after #{@max_events} events at logical time #{p.time.round(3)}"
      raise SonicPi::Stop
    end

    def default_locals
      { synth: :beep, transpose: nil, octave_shift: nil, cent_tuning: nil,
        arg_bpm_scaling: true, check_synth_args: nil, warn_unknown_opts: nil, new_thread_idx: 0 }
    end

    # Starts a thread from the current one and runs it until it first waits.
    def spawn(lang, opts, &block)
      parent = @current
      name = opts[:name]            # as given: Sonic Pi keeps "drums" and :drums apart
      if name && named_process(name, parent.wall)
        # Sonic Pi's __info goes to the GUI, not the log the trace records
        return nil
      end
      path = parent.path + [parent.spawned]
      parent.spawned += 1
      child = Process.new(path, name, parent)
      register(child, parent.uid)
      prune_threads(parent.members, parent.wall) if parent.members.size > 64
      parent.members << child
      # every with_fx block the parent is in, of its own, waits for the child
      f = parent.fx
      while f && f.owner.equal?(parent)
        prune_threads(f.members, parent.wall) if f.members.size > 64
        f.members << child
        f = f.parent
      end
      child.rand = parent.rand.child(opts[:seed])
      # what reset puts back (Sonic Pi's ThreadLocal keeps the vars a thread
      # began with): its settings, density and random stream, the source as
      # inherited, which may be none; the thread itself draws from :white then
      child.reset_to = [child.locals.dup, child.density, child.rand.dup]
      child.rand.source ||= :white
      @named[name] = child if name
      child.spawn_line = line_of(child)   # asked in the parent's fiber: the line the thread starts from
      Native.gui(:thread_start, child.uid, live_time(child), child.path[0], line_of(child), child.time.round(6), child.beat.round(6), child.id, child.name, parent.id) if @live
      delay = opts[:delay]
      sync_sym = opts[:sync_bpm] ? nil : opts[:sync]
      sync_bpm_sym = opts[:sync_bpm]
      child.fiber = Fiber.new do
        begin
          lang.sleep(delay) if delay
          lang.sync(sync_sym) if sync_sym
          lang.sync_bpm(sync_bpm_sym) if sync_bpm_sym
          block.call
        rescue SonicPi::Stop
          log_line(child, "Stopped internal thread") unless child.name
        rescue Exception => e
          child.errored = true
          record_error(child, e, child.file)
        end
        Native.gui(:thread_end, child.uid, live_time(child), child.path[0], 0, child.time.round(6), child.beat.round(6)) if @live
        [:done]
      end
      if opts[:defer]
        # A live_loop's thread spends its first moment setting up its fx in
        # Sonic Pi, and the parent wins that race: it runs on before the
        # loop's body does. So the loop starts when the parent next waits.
        child.wake_time = child.time
        child.state = :sleeping
        @queue << [child.wake_time, (@seq += 1), child]
      else
        resume(lang, child, nil)
      end
      child
    end

    # Gives a process the run until it next waits, then deals with why.
    def resume(lang, p, value)
      saved = @current
      @current = p
      SonicPi.current_process = p
      SonicPi::Rand.current = p.rand
      p.state = :running
      p.active_at = p.start + p.time
      loop do
        result = p.fiber.resume(value)
        value = nil
        case result[0]
        when :sleep
          p.state = :sleeping
          p.wake_time = result[1]
          @queue << [p.wake_time, (@seq += 1), p]
          break
        when :sync
          key, addresses = result[1], result[2]
          ev = @history.after(key, addresses)
          if ev
            value = ev          # the cue already happened later in order: carry straight on
            p.wall = ev.wall if ev.wall && ev.wall > p.wall
            next
          end
          p.state = :waiting
          @waiters << [p, key, addresses]
          break
        when :done
          p.state = p.errored ? :error : :done
          p.ended_at = p.start + p.time
          p.done = true
          p.body_end = p.wall
          # the name goes now if nothing it played still sounds; otherwise when it is next asked for
          @named.delete(p.name) if p.name && @named[p.name].equal?(p) && !holds_name?(p, p.wall)
          settle_fx
          break
        end
      end
      @current = saved
      SonicPi.current_process = saved
      SonicPi::Rand.current = saved ? saved.rand : nil
    end

    # ── Cues ──────────────────────────────────────────────────────────────

    # sync's addresses may be OSC patterns, as native's are: * (within a part), ?, [abc] or [!abc], {a,b}.
    # sync "/midi:*/note_on" takes a note from any MIDI device.
    ADDRESS_PATTERN = /[*?\[{]/
    @address_res = {}
    def self.address_re(pat)
      @address_res[pat] ||= begin
        out = +"\\A"
        i = 0
        while i < pat.size
          c = pat[i]
          case c
          when "*" then out << "[^/]*"
          when "?" then out << "[^/]"
          when "["
            j = pat.index("]", i) || pat.size
            body = pat[(i + 1)...j]
            body = "^" + body[1..] if body.start_with?("!")
            out << "[" << body.gsub("\\", "\\\\") << "]"
            i = j
          when "{"
            j = pat.index("}", i) || pat.size
            out << "(?:" << pat[(i + 1)...j].split(",").map { |alt| Regexp.escape(alt) }.join("|") << ")"
            i = j
          else out << Regexp.escape(c)
          end
          i += 1
        end
        Regexp.new(out << "\\z")
      end
    end
    def address_hit?(addresses, address)
      addresses.any? { |a| a == address || (a.match?(ADDRESS_PATTERN) && Scheduler.address_re(a).match?(address)) }
    end

    # A cue from outside the program — a MIDI message the page heard — at the moment it arrived: into the Time
    # State as a program's cue goes, waking the syncs that wait on its address. It sorts ahead of a program's own
    # cues at the same instant, as native's incoming events do.
    def external_cue(address, val, now)
      @external_seq = (@external_seq || 0) + 1
      ev = CueEvent.new(now, -100, [], @external_seq, 0, 60.0, address, val)
      ev.wall = now
      @history.add(ev)
      # A cue from outside — a MIDI controller, the host — is recorded like any other, so the GUI hears about it
      # the same way and has one inbound path for cues rather than drawing the ones it happens to have sent
      # itself. Its thread is nobody's: uid 0, no line.
      # no thread, no run, and so no time within one: the GUI leaves that column empty for a cue from outside
      Native.gui(:cue, 0, now, 0, nil, nil, 0.0, address, SonicPi.log_inspect(val)) if @live
      woken = @waiters.select { |(_, key, addresses)| address_hit?(addresses, address) && CueEvent.compare(ev.key, key) > 0 }
      @waiters -= woken
      @pending ||= {}
      woken.each do |(w, _, _)|
        w.wake_time = ev.time - w.start
        w.wall = now if now > w.wall
        @pending[w] = ev
        @queue << [w.wake_time, (@seq += 1), w]
      end
      ev
    end

    def cue(p, priority, address, val)
      ev = CueEvent.new(p.start + p.time, priority, p.path, p.delta, p.beat, p.bpm, address, val)
      p.delta += 1
      ev.wall = p.wall
      @history.add(ev)
      # every cue to the GUI's cue log, a live loop's each time round included, as native lists them
      Native.gui(:cue, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(6), p.beat.round(6), address, SonicPi.log_inspect(val)) if @live
      woken = @waiters.select { |(_, key, addresses)| address_hit?(addresses, address) && CueEvent.compare(ev.key, key) > 0 }
      @waiters -= woken
      @pending ||= {}
      woken.each do |(w, _, _)|
        w.wake_time = ev.time - w.start
        w.wall = p.wall if p.wall > w.wall
        @pending[w] = ev
        @queue << [w.wake_time, (@seq += 1), w]
      end
      ev
    end

    # RT: what no waiting or sleeping thread can still sync on goes, address by
    # address; the newest cue at each address stays, for get.
    #
    # The horizon is the earliest moment a thread can still be at: no thread is
    # before it, so no sync or get can ask about a cue older than it. With
    # nothing queued and nobody waiting there is no such moment, and nothing
    # running to ask at all — then only the newest at each address is worth
    # keeping, since the next run starts after every cue here.
    def prune_history!
      horizon = nil
      @queue.each { |(_, _, p)| t = p.start + p.time; horizon = t if horizon.nil? || t < horizon }
      @waiters.each { |(_, key, _)| horizon = key[0] if horizon.nil? || key[0] < horizon }
      @history.trim!(horizon)
    end

    # What a woken sync receives when its process runs again.
    def take_pending(p)
      return nil unless @pending
      @pending.delete(p)
    end

    def wait_for_cue(key, addresses)
      ev = Fiber.yield([:sync, key, addresses])
      ev || take_pending(@current)
    end

    # ── live_loop scopes (Sonic Pi gives each live loop a scope slot) ─────

    def scope_slot(ll_name)
      return @scope_slots[ll_name] if @scope_slots[ll_name]
      used = @scope_slots.values
      slot = (10..31).find { |s| !used.include?(s) }
      @scope_slots[ll_name] = slot if slot
      slot
    end

    def release_scope_slot(ll_name) = @scope_slots.delete(ll_name)

    # ── The trace ─────────────────────────────────────────────────────────

    # RT sends each record as OSC (Native.gui: the thread's uid, the record's
    # time on the host's clock, its job and line, then the kind's own fields,
    # which web/gui-stream.js reads back); NRT keeps it for the trace.
    # A sound in a with_fx says which fx it goes into (`fx`: that fx's
    # trigger); an fx starting says the same of the fx around it. frame: the
    # new fx's own, when this is one.
    # A trace's frame: what is heard is timed by when it is heard, counted from the run's beat grid (its start and the
    # default schedule-ahead), so a trace is the same whatever the default: a thread on the default is at its own
    # logical time, one with a schedule-ahead of its own (use_real_time, use_sched_ahead_time) that much off it. The
    # oracle records its traces in the same frame (oracle/harness/oracle.rb). What isn't heard (a cue, a load, a log)
    # stays on the thread's clock.
    def trace_t(p) = (p.time + p.sched_ahead - DEFAULT_SCHED_AHEAD).round(6)
    # a synth or fx named by its trigger (a control's of:, a sound's fx:), in the same frame; its schedule-ahead dropped
    def trace_ref(r)
      return r unless r.is_a?(Hash) && r.key?(:sa)
      out = r.reject { |k, _| k == :sa }
      out[:t] = (r[:t] + r[:sa] - DEFAULT_SCHED_AHEAD).round(6) if r[:sa]
      out
    end

    def record_event(p, h, node = nil, frame = nil)
      p.events += 1
      into = p.fx && p.fx.ref
      if @live
        # the last field: a real-time thread's sound, which goes immediately (audio_time)
        Native.gui(:synth, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(6), p.beat.round(6), h[:synth], h[:now], node, h[:args],
                   into && into[:t], into && into[:thread], into && into[:synth], p.sched_ahead.zero?)
        audio_s_new(p, h, node, frame) if node
      else
        ev = { kind: "synth", t: trace_t(p), beat: p.beat.round(6), thread: p.id, name: p.name.to_s }.merge(h)
        ev[:fx] = trace_ref(into) if into
        @events << ev
      end
      check_cap!(p)
    end

    def next_node_id = (@node_id += 1)

    # A control or kill names the synth it acts on by that synth's trigger:
    # its logical time and thread (and, live, its node id).
    def record_control(p, node, args)
      if @live
        Native.gui(:control, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(6), p.beat.round(6), node.name, node.ref[:t], node.ref[:thread], node.id, args, p.sched_ahead.zero?)
        audio_n_set(p, node, args)
      else
        @events << { kind: "control", t: trace_t(p), beat: p.beat.round(6), thread: p.id, name: p.name.to_s, synth: node.name, of: trace_ref(node.ref), args: args }
      end
      check_cap!(p)
    end

    def record_kill(p, node)
      # the synth ends when the kill reaches the engine, if it had not already
      at = p.start + p.time + p.sched_ahead
      node.ends_at = at if node.ends_at && at < node.ends_at
      if @live
        Native.gui(:kill, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(6), p.beat.round(6), node.name, node.ref[:t], node.ref[:thread], node.id, p.sched_ahead.zero?)
        audio_n_free(p, node)
      else
        @events << { kind: "kill", t: trace_t(p), beat: p.beat.round(6), thread: p.id, name: p.name.to_s, synth: node.name, of: trace_ref(node.ref) }
      end
    end

    # a node paused or run again (native's node.pause and node.run): the engine's /n_run when live; a trace keeps no
    # event for it, as the oracle's does not
    def record_run(p, node, on)
      Native.n_run(audio_time(p), NODE_BASE + node.id, on ? 1 : 0) if @live && node.id
    end

    def record_midi(p, path, args)
      p.events += 1
      if @live
        Native.gui(:midi, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(6), p.beat.round(6), path, args)
      else
        @events << { kind: "midi", t: trace_t(p), beat: p.beat.round(6), thread: p.id, name: p.name.to_s, path: path, args: args }
      end
      check_cap!(p)
    end

    # RT only: what the page shows about a run as it happens (threads
    # starting and ending, sleeps, syncs, cues, studio settings). A trace
    # does not keep these.
    def rt_record(p, h)
      return unless @live
      Native.gui(:record, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(6), p.beat.round(6), h)
    end

    def rt_sleep(p, from, beats)
      return unless @live
      p.wait_line = line_of(p)
      Native.gui(:sleep, p.uid, p.start + from + p.sched_ahead + @time_warp, p.path[0], p.wait_line, from.round(6), p.beat.round(6), beats, p.time.round(6))
    end

    def rt_sync(p, addresses)
      return unless @live
      p.wait_line = line_of(p)
      Native.gui(:sync, p.uid, live_time(p), p.path[0], p.wait_line, p.time.round(6), p.beat.round(6), addresses)
    end

    # A record's time on the host's clock: when it sounds.
    def live_time(p) = p.start + p.time + p.sched_ahead + @time_warp
    # the engine's output level as it last said (/sonic-pi/amp: [time, amp]): current_amp; [0.0, 1.0] until it does
    def amp = @amp || [0.0, 1.0]
    attr_writer :amp
    # how far a thread's next sound is behind the host's clock, in seconds (negative: in good time), as native's
    # time_diff; a trace has no clock, and is never behind
    def behind(p) = @live && @now ? @now - live_time(p) : -1.0

    # The line a thread is at in the program: its fiber's frame there, or, when it has none (a sample's loading
    # thread runs the language's own block), the line it was started from.
    def line_of(p) = @line_lookup ? (SonicPi::Native.line(p.file) || p.spawn_line) : nil

    # ── with_fx (see FxFrame) ─────────────────────────────────────────────

    # A with_fx block begins, inside whatever the thread is in now. Live, it
    # gets its groups and its bus (none left: the block plays without it).
    def open_fx(p)
      f = FxFrame.new(p, p.fx)
      if @live
        f.group = next_node_id
        f.synths = next_node_id
        f.bus = alloc_bus(p.start + p.time)
        f.uid = (@uid += 1)
        f.line = line_of(p)
        f.opened_at = p.start + p.time
      end
      @fx_open << f
      f
    end

    # The fx is running: from here the block's sounds and threads go into it.
    def enter_fx(p, f, node)
      f.node = node
      f.ref = { t: node.ref[:t], thread: node.ref[:thread], synth: node.name, sa: node.ref[:sa] }
      p.fx = f
    end

    def close_fx(p, f)
      f.block_end = p.wall
      p.fx = f.parent
      @fx_pending << f
      settle_fx
    end

    # A sound the thread made: its thread, and the with_fx block the thread
    # is in of its own, wait for it to end.
    def track(p, node)
      @sounds << [(@uid += 1), p, p.fx, node, line_of(p), live_time(p)] if @live && !node.frame
      prune_nodes(p.nodes, p.wall) if p.nodes.size > 64
      p.nodes << node
      f = p.fx
      return unless f && f.owner.equal?(p)
      prune_nodes(f.nodes, p.wall) if f.nodes.size > 64
      f.nodes << node
    end

    # What has ended by a thread's wall clock can no longer decide when it or
    # its with_fx is done, which is later still.
    def prune_nodes(nodes, wall) = nodes.reject! { |n| (e = node_end(n)) && e <= wall }
    def prune_threads(threads, wall) = threads.reject! { |c| (e = subtree_end(c)) && e <= wall }

    def node_end(n) = n.frame ? n.frame.node_end : n.ends_at

    # When a thread is done and every thread it started is too.
    def subtree_end(p)
      return p.subtree_at if p.subtree_at
      t = p.body_end or return nil
      p.nodes.each { |n| e = node_end(n) or return nil; t = e if e > t }
      p.members.each { |c| e = subtree_end(c) or return nil; t = e if e > t }
      p.subtree_at = t
    end

    def fx_free_at(f)
      t = subtree_end(f) or return nil   # the same walk a thread's end takes: its block, its sounds, its threads
      t = f.released_at if f.released_at && f.released_at > t
      f.free_at = t + f.kill_delay
    end

    # Frees every ended block that can be, and again while a free lets an
    # outer one go.
    def settle_fx
      loop do
        freed = @fx_pending.select { |f| fx_free_at(f) }
        break if freed.empty?
        @fx_pending -= freed
        @fx_open -= freed
        freed.each { |f| fx_freed(f) }
      end
    end

    # The free is on the wall clock, which the oracle only nears (a few
    # milliseconds late), so specs match its time within a tolerance. Past a
    # horizon it is not recorded, as the oracle stops before it.
    def fx_freed(f)
      o = f.owner
      t = (f.free_at - o.start).round(6)      # on its job's clock, as a trace's times are
      shown = f.ref && !(@stop_after && t - DEFAULT_SCHED_AHEAD > @stop_after)   # the horizon, in the trace's frame (the oracle's)
      if @live
        at = f.free_at + o.sched_ahead + @time_warp   # heard as its thread's sounds are
        at = f.free_not_before if f.free_not_before && f.free_not_before > at
        @fx_frees << [at, f] if f.group
        @fx_gone << f if f.uid
        # in the trace's frame, as the NRT event below: the page times it by `at`; t and of are the trace's own
        Native.gui(:fx_free, o.uid, at, o.path[0], nil, (t - DEFAULT_SCHED_AHEAD).round(6), trace_ref(f.ref)[:t], f.ref[:thread], f.ref[:synth], f.first_node || f.node.id) if shown
      elsif shown
        # heard at its wall-clock moment: in the trace's frame, less the default schedule-ahead (trace_t)
        @events << { kind: "fx_free", t: (t - DEFAULT_SCHED_AHEAD).round(6), thread: o.id, name: o.name.to_s, synth: f.ref[:synth], of: trace_ref(f.ref) }
      end
    end

    # This runtime's rule, not native's (which leaves a running loop where it is): a
    # live_loop run again from inside another with_fx, or from outside any,
    # moves there. The fx it was in stops waiting for it, the one around the
    # call waits instead, and the loop's own fx (its scope's, and any its
    # body is inside) move with it, so its sound follows.
    def move_loop(loop, caller)
      return if loop.done
      own = nil
      from = loop.fx
      while from && from.owner.equal?(loop)
        own = from
        from = from.parent
      end
      to = caller.fx
      return if from.equal?(to)
      # the fx it leaves stops waiting for it; live, its engine group is not freed before the handover out of it is
      # over (fx_freed): freed sooner, it would take the loop's synths group with it, and every sound after would go
      # to a dead node. The record keeps the trace's time.
      @fx_open.each do |f|
        next unless f.members.delete(loop)
        f.subtree_at = nil
        f.released_at = caller.wall if f.released_at.nil? || caller.wall > f.released_at
        f.free_not_before = live_time(caller) + HANDOVER + 0.1 if @live
      end
      # the fx the loop was in through a thread that started it wait for it no longer either
      q = loop.parent
      while q
        q.subtree_at = nil
        q = q.parent
      end
      g = to
      while g && g.owner.equal?(caller)
        g.members << loop
        g.subtree_at = nil   # a loop moved in: the block is not over while it plays
        g = g.parent
      end
      loop.parent.members.delete(loop) if loop.parent
      caller.members << loop
      loop.parent = caller
      loop.parent_uid = caller.uid
      regroup(loop, caller.group)
      own ? own.parent = to : loop.fx = to
      if @live
        into = sink(to)
        # When the fx it moves into starts: a Run's sounds, that fx among them,
        # are scheduled sched_ahead out, and moved at once the loop would play
        # into a bus nothing reads until then (a hit cut, the next one lost).
        handover_fx(own, into, live_time(caller)) if own && own.bus && own.node
        Native.gui(:loop_move, caller.uid, live_time(caller), caller.path[0], line_of(caller), caller.time.round(6), caller.beat.round(6), loop.id,
                   to && to.ref && to.ref[:t], to && to.ref && to.ref[:thread], to && to.ref && to.ref[:synth], loop.uid)
      else
        ev = { kind: "loop_move", t: caller.time.round(6), beat: caller.beat.round(6), thread: caller.id, name: caller.name.to_s, loop: loop.id }
        ev[:fx] = trace_ref(to.ref) if to
        @events << ev
      end
      settle_fx
    end

    # The move made inaudible. A loop's sound leaves through its own fx's one
    # node (its scope_out); switching that node's out_bus is a step in the
    # waveform on both sides — the old fx's input cut mid-cycle, the new fx
    # starting mid-cycle with fresh state — heard as a click. So a second
    # scope_out is made in the new place, reading the same bus, and the two
    # cross over in HANDOVER seconds as the new fx starts; then the old node
    # goes, and the loop's synths group moves under the new one, ahead of it,
    # before the fx it was in is freed. The page keys the loop's scope on the
    # node it first saw (first_node), so the slot carries on unbroken.
    HANDOVER = 0.03
    def handover_fx(own, into, at)
      out = into ? into.bus : JOB_BUS
      target = into ? NODE_BASE + into.synths : STUDIO_SYNTHS
      old_node, old_group = own.node, own.group
      group = next_node_id
      node = SynthNode.new(old_node.name, old_node.args, old_node.info, next_node_id, old_node.ref)
      node.frame = own
      own.first_node ||= old_node.id
      # the new node now, silent, so a sound the loop makes before the fade finds its place
      Native.g_new(IMMEDIATE, NODE_BASE + group, 1, target)
      Native.s_new(IMMEDIATE, EngineIds.synthdef(node.name), -1, node.name, NODE_BASE + node.id,
                   node.args.merge("in_bus" => own.bus, "out_bus" => out, "amp" => 0.0), 1, NODE_BASE + group)
      Native.n_set(at, -1, NODE_BASE + node.id, { "amp" => (node.args["amp"] || 1.0).to_f, "amp_slide" => HANDOVER })
      Native.n_set(at, -1, NODE_BASE + old_node.id, { "amp" => 0.0, "amp_slide" => HANDOVER })
      later = at + HANDOVER + 0.05
      Native.n_order(later, 0, NODE_BASE + group, NODE_BASE + own.synths)
      Native.n_free(later, NODE_BASE + old_node.id)
      Native.n_free(later, NODE_BASE + old_group)
      own.group = group
      own.node = node
    end

    # ── The audio stream (RT): each sound as the OSC the engine plays ─────
    #
    # Sent as the record is made, for the record's own time. Sonic Pi's node
    # tree: a sound in a with_fx plays into that block's bus from the tail of
    # its synths group; the block's own group is at the tail of the group of
    # whatever it is in (the studio's synths group at the top), with its
    # synths group at its head and the fx after them. Groups and an fx that
    # starts now go at the thread's moment, so they are there before the
    # block's first sound.
    #
    # Nothing plays straight to the speakers. As in Sonic Pi's studio, a run's
    # sounds go to its bus, a basic_mixer takes that down to 0.3 into the
    # mixer bus, and sonic-pi-mixer (pre_amp, filters, limiter) plays that
    # out; the mixers are in a group after the synths. Sonic Pi gives each
    # run its own bus and basic_mixer; mixers sum, so one of each for every
    # run sounds the same.
    NODE_BASE = 10000     # the runtime's node ids, clear of SuperSonic's own
    BUS_FIRST = 16        # the busses fx use, clear of the engine's ins and outs
    STUDIO_SYNTHS = 1001  # the group every run's sounds and fx are in
    STUDIO_MIXER = 1000   # the group the mixers are in, after it
    MIXER_NODE = 1002     # sonic-pi-mixer
    JOB_MIXER_NODE = 1003 # sonic-pi-basic_mixer
    MIXER_BUS = 10        # into sonic-pi-mixer
    JOB_BUS = 12          # every run's sounds, into the basic_mixer
    JOB_MIXER_AMP = 0.3   # Lang::Sound#job_mixer's

    # The studio, before a session's first sound and again after a Stop (the
    # page frees every node in the engine then).
    def ensure_studio(now)
      return if @studio
      @studio = true
      Native.g_new(studio_time(now), STUDIO_SYNTHS, 0, 0)
      Native.g_new(studio_time(now), STUDIO_MIXER, 3, STUDIO_SYNTHS)
      Native.s_new(studio_time(now), EngineIds.synthdef("sonic-pi-mixer"), -1, "sonic-pi-mixer", MIXER_NODE,
                   { "in_bus" => MIXER_BUS, "amp" => @volume.to_f, "pre_amp" => @drive.to_f }, 0, STUDIO_MIXER)
      Native.s_new(studio_time(now), EngineIds.synthdef("sonic-pi-basic_mixer"), -1, "sonic-pi-basic_mixer", JOB_MIXER_NODE,
                   { "in_bus" => JOB_BUS, "out_bus" => MIXER_BUS, "amp" => JOB_MIXER_AMP }, 0, STUDIO_MIXER)
    end
    BUS_LAST = 1024       # as many as web/sonic_pi.js boots SuperSonic with
    FREE_LEAD = 1.0       # an fx's free leaves this long before it is due

    # the buffer a sound names by its file: a player's sample (buf), or the random stream a synth tosses its coins
    # with (rand_buf, which no player takes). The host sends it as its number, and the sound waits for it to load.
    def audio_buffer(args)
      file = args["buf"] || args[:buf] || args["rand_buf"] || args[:rand_buf]
      file.is_a?(String) ? EngineIds.buffer(file) : -1
    end

    # The block whose bus a sound goes into: the nearest that has one.
    def sink(f)
      f = f.parent while f && !f.bus
      f
    end

    # The time for a message that goes at a thread's moment rather than a
    # schedule-ahead after it (a block's groups, an fx that starts now, a
    # loop moving): immediately (0 is OSC's "immediately", host/sp_host.c),
    # as Sonic Pi sends them. Stamped with the thread's moment they would
    # reach the engine a few milliseconds after it and every one count as
    # late, a with_fx inside a live_loop adding a few each time round.
    # Immediate bundles run in the order they arrive, so a move still
    # follows the groups it moves into.
    IMMEDIATE = 0.0
    def studio_time(_now) = IMMEDIATE

    # A sound's time on the wire: a schedule-ahead after its thread's moment, or, for a thread that asked for real
    # time (use_real_time, with_real_time: no schedule-ahead), immediately. Its moment is now: stamped with it, the
    # bundle would reach the engine a moment after and every note count as late (a docs keyboard's, a MIDI
    # controller's), so the count would not say when something really is late. It sounds the same: at once.
    def audio_time(p) = p.sched_ahead.zero? ? IMMEDIATE : live_time(p)

    def alloc_bus(now)
      i = @busses.index { |(_, from)| from <= now }
      return @busses.delete_at(i)[0] if i
      return nil if @next_bus + 1 >= BUS_LAST
      bus = @next_bus
      @next_bus += 2
      bus
    end

    def audio_s_new(p, h, node, frame = nil)
      synth = h[:synth]
      args = h[:args]
      now = p.start + p.time
      ensure_studio(now)
      into = sink(p.fx)
      out = into ? into.bus : JOB_BUS
      target = into ? NODE_BASE + into.synths : STUDIO_SYNTHS
      if frame
        return unless frame.bus
        Native.g_new(studio_time(now), NODE_BASE + frame.group, 1, target)
        Native.g_new(studio_time(now), NODE_BASE + frame.synths, 0, NODE_BASE + frame.group)
        Native.s_new(h[:now] ? studio_time(now) : audio_time(p), EngineIds.synthdef(synth), audio_buffer(args), synth, NODE_BASE + node,
                     args.merge("in_bus" => frame.bus, "out_bus" => out), 1, NODE_BASE + frame.group)
      else
        # inside a block, at the tail of its synths; outside any, at the head of the studio's
        Native.s_new(audio_time(p), EngineIds.synthdef(synth), audio_buffer(args), synth, NODE_BASE + node, args.merge("out_bus" => out), into ? 1 : 0, target)
      end
    end

    def audio_n_set(p, node, args)
      return unless args.is_a?(Hash)
      Native.n_set(audio_time(p), audio_buffer(args), NODE_BASE + node.id, args)
    end

    def audio_n_free(p, node)
      Native.n_free(audio_time(p), NODE_BASE + node.id)
    end

    # Frees due within the lead leave, and their busses come back once the
    # free has reached the engine.
    def send_fx_frees(now)
      due, @fx_frees = @fx_frees.partition { |(at, _)| at - FREE_LEAD <= now }
      due.each do |(at, f)|
        Native.n_free(at, NODE_BASE + f.group)
        @busses << [f.bus, at] if f.bus
      end
      nodes, @node_frees = @node_frees.partition { |(at, _)| at - FREE_LEAD <= now }
      nodes.each { |(at, id)| Native.n_free(at, NODE_BASE + id) }
    end

    def free_fx_now(f)
      return unless @live && f.group
      Native.n_free(0.0, NODE_BASE + f.group)
      @busses << [f.bus, 0.0] if f.bus
    end

    # A sample freed: the page hears of it, and the engine lets its buffer go.
    def sample_freed(p, file)
      rt_record(p, { kind: "sample_free", path: file })
      return unless @live
      n = EngineIds.free_buffer(file)
      Native.host("/sonic-pi/sample_free", n, file) if n
    end

    # The latest value set for one of these addresses strictly before this
    # thread's moment (Sonic Pi's time state).
    def get_event(p, addresses)
      @history.before(p.key, addresses)
    end

    # run_code: the code as a job of its own, starting at the caller's moment.
    def run_code(lang, code, from)
      saved = @current
      @next_job ||= 1
      id = @next_job
      @next_job += 1
      file = "run_code-#{id}"
      remember_source(file, code)
      start_job_process(id, from.start + from.time, file) { lang.instance_eval(SonicPi::PreParser.preparse(code), file, 1) }
      @current = saved
      SonicPi.current_process = saved
      SonicPi::Rand.current = saved ? saved.rand : nil
      id
    end

    def record_load(p, path)
      if @live
        Native.gui(:sample_load, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(6), p.beat.round(6), path)
        EngineIds.buffer(path)
      else
        @events << { kind: "sample_load", t: p.time.round(6), beat: p.beat.round(6), thread: p.id, name: p.name.to_s, path: path }
      end
    end

    # live_audio (lang_more.rb): a synth that lasts until stopped, one a name, kept across runs. Its place is the fx
    # it sounds into (none: the run's bus); called again from another, it moves there rather than starting over, as
    # native's trigger_live_synth does: its out_bus set to the new fx's, and the node put at the tail of that fx's synths.
    def live_place(p) = sink(p.fx)

    def live_move(p, node, place)
      return unless @live
      at = audio_time(p)
      Native.n_set(at, -1, NODE_BASE + node.id, { "out_bus" => (place ? place.bus : JOB_BUS).to_f })
      Native.n_order(at, place ? 1 : 0, place ? NODE_BASE + place.synths : STUDIO_SYNTHS, NODE_BASE + node.id)
    end

    # The page opens the sound card's input (getUserMedia) for live_audio: this many channels, from input 1
    def record_audio_in(p, channels)
      if @live
        Native.host("/sonic-pi/audio-in", channels, "")
      else
        @events << { kind: "audio_in", t: p.time.round(6), beat: p.beat.round(6), thread: p.id, name: p.name.to_s, channels: channels }
      end
    end

    # A synthdef from a URL (load_synthdef): the page fetches it and hands it to the engine (sonic_pi.js)
    def record_synthdef_load(p, url)
      if @live
        Native.host("/sonic-pi/synthdef-url", 0, url)
      else
        @events << { kind: "synthdef_load", t: p.time.round(6), beat: p.beat.round(6), thread: p.id, name: p.name.to_s, url: url }
      end
    end

    # Output and log lines share Sonic Pi's message path, whose run time is
    # rounded to four places for the GUI.
    def output_line(p, text)
      if @live
        Native.gui(:output, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(4), text)
      else
        @output << { t: p.time.round(4), thread: p.id, name: p.name.to_s, text: text }
      end
    end

    def log_line(p, text)
      if @live
        Native.gui(:log, p.uid, live_time(p), p.path[0], line_of(p), p.time.round(4), text)
      else
        @log << { t: p.time.round(4), thread: p.id, name: p.name.to_s, text: text }
      end
    end

    # a run's code by its file (run-3), for the column of a syntax error in it (record_error); the last few only.
    # Kept here, not in a wrapper round the eval: code evaluated from a string sees the locals of the method that
    # evaluates it, so a wrapper's own names (at, e) would become the program's.
    def remember_source(file, code)
      return unless @live
      (@sources ||= {})[file] = code
      @sources.delete(@sources.keys.first) while @sources.size > 16
    end

    def record_error(p, e, file = p.file)
      line = -1
      (e.backtrace || []).each do |l|
        l = l.split(" ")[0] || ""
        l = l.split("] ")[-1]
        next unless l.start_with?(file + ":")
        line = l[(file.size + 1)..].split(":")[0].to_i
        break
      end
      message = e.message.split("\n")[0].to_s.strip
      # the preparser's refusal names the line in its words ("… (line 3)"): the error card points at it
      line = $1.to_i if e.is_a?(SonicPi::PreParser::PreParseError) && e.message =~ /\(line (\d+)\)/
      # mruby's eval says a syntax error's line only; its parser has the column too, which the error card's caret needs:
      # "line 4: …" becomes "line 4:54: …" (1-based)
      if @live && e.is_a?(SyntaxError) && (src = @sources && @sources[file]) && (at = Native.syntax_at(src))
        message = message.sub(/line \d+: /, "line #{at[0]}:#{at[1]}: ")
      end
      if e.is_a?(NameError)                   # the interpreter's wording, cut to the name (see the oracle harness)
        cut = message.index(" for ")
        message = message[0, cut] if cut
        message = message.tr("`", "'")
      end
      if @live
        # an opt that broke a rule carries the rule itself (SonicPi::OptError): the error card says it in its own
        # words and can offer a value that would do, rather than reading this sentence back apart
        Native.gui(:error, p.uid, live_time(p), p.path[0], line, e.class.name, message,
                   e.is_a?(SonicPi::OptError) ? e.fault : nil)
      else
        @errors << { class: e.class.name, message: message, line: line, thread: p.id, name: p.name.to_s }
      end
    end
  end
end
