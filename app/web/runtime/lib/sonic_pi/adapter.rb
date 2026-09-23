# SPDX-License-Identifier: AGPL-3.0-or-later
# The runtime as an adapter for scripts/check.rb: runs a program and gives
# back its trace as JSON. The same code serves the CLI (bin/trace.rb) and the
# wasm host (host/sp_host.c).
module SonicPi
  class << self
    attr_accessor :current_lang
  end

  class Adapter
    # Within one thread, order is the program's own; across threads at one
    # instant the thread's path decides (as the oracle's trace does). An fx's
    # free comes after what its thread did at that instant: it is made on the
    # wall clock, which is behind.
    def self.ordered(items)
      items.each_with_index.sort_by { |e, i| [e[:t], e[:thread].to_s, e[:kind] == "fx_free" ? 1 : 0, i] }.map { |e, _| e }
    end

    # max_time: how far into logical time a program that never ends is
    # traced; max_events: the cap on a thread that never sleeps.
    def self.trace(code, file, max_time = 30.0, max_events = 20000)
      SonicPi::Samples.start_run!
      SonicPi.link_bpm = 60.0
      SonicPi.link_origin = nil
      SonicPi.link_anchor = nil
      SonicPi.sched_ahead = nil
      sched = SonicPi::Scheduler.new
      sched.max_time = max_time
      sched.max_events = max_events
      # A spec may give a horizon in its leading comments: `# horizon: 2.001`.
      code.split("\n").each do |l|
        break unless l.start_with?("#")
        sched.stop_after = l[10..].to_f if l.start_with?("# horizon:")
      end
      lang = SonicPi::Language.new(sched)
      was = SonicPi.current_lang
      SonicPi.current_lang = lang
      # the preparser refuses a program before any of it runs, as native's does: the run's own error, no thread's
      begin
        code = SonicPi::PreParser.preparse(code)
      rescue SonicPi::PreParser::PreParseError => e
        sched.errors << { class: e.class.name, message: e.message.split("\n")[0].strip, line: -1, thread: "0", name: "" }
        return JSON.generate({ events: [], errors: sched.errors, output: [], log: [] })
      end
      sched.run(lang, SonicPi::Rand.tables, file) do
        lang.instance_eval(code, file, 1)
      end
      SonicPi.current_lang = was
      errors = sched.errors.sort_by { |e| [e[:line].to_i, e[:thread], e[:class]] }
      JSON.generate({ events: ordered(sched.events), errors: errors, output: ordered(sched.output), log: ordered(sched.log) })
    end
  end

  # RT: one language and one scheduler for a whole session, ticked by the
  # host's clock; every record leaves as OSC as it is made (host/sp_host.c).
  # Runs are jobs: a new Run does not stop the last, and its live_loops
  # take over the running ones by name, as in Sonic Pi.
  module Live
    class << self
      def boot_native
        SonicPi::Samples.start_run!         # a session starts with nothing loaded
        SonicPi.link_bpm = 60.0
        SonicPi.link_origin = nil
        SonicPi.link_anchor = nil
        SonicPi.sched_ahead = nil
        @sched = SonicPi::Scheduler.new
        @sched.live = true
        @lang = SonicPi::Language.new(@sched)
        SonicPi.current_lang = @lang
        @runs = 0
      end

      # a run, in a group (0: none in particular): a card's runs share one, a buffer's another (Scheduler#stop_group)
      def run(code, now, group = 0)
        file = "run-#{@runs += 1}"
        @sched.remember_source(file, code)
        @sched.start_job(@lang, SonicPi::Rand.tables, now, file, group) { @lang.instance_eval(SonicPi::PreParser.preparse(code), file, 1) }
      end

      def tick(now) = @sched.step(now)
      # the page's Link strip: the tempo (changing at `at`, as it sounds) and the global time warp (seconds)
      def set_link_bpm(bpm, at) = @sched.set_link_bpm(bpm, at)
      # a cue from outside (MIDI in): its address, its values as "60,100", at `now` on the session's clock
      def external_cue(address, vals, now) = @sched.external_cue(address, host_values(vals), now)
      # a host cue's values, each tagged with its kind (web/live-core.js cue): i an integer, f a float, b a boolean,
      # s a string, a unit separator between them
      def host_values(vals)
        vals.to_s.split("\x1f").map do |v|
          x = v[1..] || ""
          case v[0]
          when "i" then x.to_i
          when "f" then x.to_f
          when "b" then x == "1"
          else x
          end
        end
      end
      def time_warp=(seconds)
        @sched.time_warp = seconds.to_f
      end
      # the host lost this much time: the schedule moves on by it
      def hold(seconds) = @sched.hold(seconds.to_f)
      def stop_all = @sched.stop_all
      def stop_job(id) = @sched.stop_job(id)
      def stop_group(group, fade, now) = @sched.stop_group(group, fade, now)
      def group_under(group, parent) = @sched.group_under(group, parent)
      def stop_subtree(uid, fade, now) = @sched.stop_subtree(uid, fade, now)
      def process_table(now) = @sched.process_table(now)

      def stop_after=(seconds)
        @sched.stop_after = seconds
      end
      def idle? = @sched.idle?
    end
  end

  # Just enough JSON to print a trace.
  module JSON
    def self.generate(v)
      case v
      when Hash then "{" + v.map { |k, x| "#{generate(k.to_s)}:#{generate(x)}" }.join(",") + "}"
      when Array then "[" + v.map { |x| generate(x) }.join(",") + "]"
      when String then '"' + v.gsub("\\") { "\\\\" }.gsub('"') { '\\"' }.gsub("\n") { "\\n" }.gsub("\t") { "\\t" } + '"'
      when Float then (v.nan? || v.infinite?) ? "null" : FloatFormat.to_s(v)
      when nil then "null"
      when true, false then v.to_s
      when Integer then v.to_s
      when Symbol then generate(v.to_s)
      else generate(v.to_s)
      end
    end
  end
end
