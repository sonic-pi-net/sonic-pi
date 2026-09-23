# Specs

Each `specs/<verb>/<name>.rb` is a Sonic Pi program. Its first comment says
what it pins down. Beside it, `<name>.expected.json` is the trace Sonic Pi's
own Ruby runtime produces for it (the oracle in `oracle/`). A new runtime
passes a spec when it produces the same trace.

Regenerate the expected traces with `scripts/gen-expected.rb`; check a runtime
with `ADAPTER="<command>" scripts/check.rb`. With no adapter, `check.rb` runs
the oracle again, which proves the specs are deterministic.

## The trace

```json
{
  "events": [ { "kind": "synth", "t": 0.5, "beat": 0.5, "thread": "0.1", "name": "bass",
                "synth": "sonic-pi-beep", "args": { "note": 36.0 }, "now": false },
              { "kind": "sample_load", "t": 0.0, "beat": 0.0, "thread": "0", "name": "",
                "path": "loop_amen.flac" } ],
  "errors": [ { "class": "RuntimeError", "message": "boom", "line": 4, "thread": "0.0", "name": "" } ],
  "output": [ { "t": 0.0, "thread": "0", "name": "", "text": "\"hello\" 1" } ],
  "log":    [ { "t": 0.0, "thread": "0", "name": "", "text": "synth :beep, {note: 60.0}" } ]
}
```

* `t` is logical time in seconds from the start of the run; `beat` is the
  thread's beat count. Neither is wall-clock time. What is heard (a `synth`,
  a `control`, a `kill`, `midi`, an `fx_free`) is timed by when it is heard,
  counted from the run's beat grid: the run's start plus the default
  schedule-ahead (`SonicPi::DEFAULT_SCHED_AHEAD`, in
  `runtime/lib/sonic_pi/defaults.rb`, which the oracle loads too). A thread
  on the default is at its own logical time; one with a schedule-ahead of its
  own (`use_real_time`, `use_sched_ahead_time`) is that much off it, as is
  every `of` and `fx` naming a sound it made. So a trace is the same whatever
  the default is, and one that changes is a change in what is heard. Cues,
  loads and the log stay on the thread's clock.
* `thread` is the thread's spawn path, as Sonic Pi numbers it. The run is
  thread `"0"`; the program's own body runs in `"0.0"` (Sonic Pi runs it as
  the run's first thread), the threads it spawns are `"0.0.0"`, `"0.0.1"`, …
  in spawn order, their children `"0.0.1.0"` and so on. `name` is the name given to
  `in_thread`/`live_loop` (a live_loop's is `live_loop_<name>`), `""` when
  there is none.
* `events` are what reached the audio server: every synth trigger with the
  opts the language resolved (bus numbers removed, buffers named by their
  sample file), and every sample load. Within a thread, events keep the
  program's own order. Threads reaching one instant together have no defined
  order, so at one instant the thread path orders them.
* `events` also carry what a program did to a running synth and to MIDI:
  `control` (`synth`, `of`: the trigger's `t` and `thread`, `args`), `kill`
  (`synth`, `of`) and `midi` (`path` such as `/note_on`, `args` with the port
  and channel first). The oracle's harness records a control or kill only
  when the program called it.
* A sound played inside a `with_fx` carries `fx`: that fx's trigger (`t`,
  `thread`, `synth`), and an fx inside another carries the outer one's.
  Every `live_loop` runs inside the `sonic-pi-fx_scope_out` fx Sonic Pi gives
  it, so its sounds name that. `fx_free` (`synth`, `of`) is the moment the
  fx is freed: once its block has ended, every thread the block started has
  ended with all of theirs (each only when its own sounds have), and every
  sound the block's own thread made has ended (a nested fx when it is freed),
  plus its kill_delay, heard at that moment (so less the default schedule-ahead,
  as above). Sonic Pi measures all of that on the wall clock, where
  a sleep wakes 0.2s early and one under 0.2s from now does not sleep, so the
  oracle's `fx_free` is a few milliseconds late on the time meant, and
  `check.rb` matches its `t` within 0.03s. A spec should keep those moments
  clear of a sleep's 0.2s boundary, and of a horizon.
* `loop_move` is the runtime's own, never the oracle's: a running `live_loop`
  run again from inside another `with_fx` (or from outside any) moves there,
  as in Tau, where Sonic Pi leaves it where it is. `loop` is the loop's
  thread, `fx` the block it joins (absent: none).
* `errors` are the exceptions the runtime reported, raw class and first
  message line, with the buffer line.
* `output` is what `puts`/`print` showed the user.
* `log` is everything else the runtime told the user (synth echoes, warnings).
  `check.rb` reports differences in it but only fails on them with
  `STRICT_LOG=1`.

Programs run in real time in the oracle, so keep each one under about two
seconds of logical time. Avoid anything that depends on the wall clock, the
machine, or an audio device.

A sample that is not loaded yet is triggered from a helper thread Sonic Pi
spawns for the load (its path is a child of the caller's), and until that
thread has run the caller still sees the sample as unloaded. Several
same-instant triggers of one unloaded sample therefore race, in Sonic Pi
itself, over which thread loads it. A spec must not pin that race: load the
sample first with `load_sample`, or sleep between the first trigger and the
next. `sample/first_trigger_thread.rb` pins the helper-thread behaviour on
its own.

## At twice the tempo

`specs/bpm/` is every spec again at twice its tempo, written by
`scripts/gen-bpm-specs.rb`: each copy sets `use_bpm 120` after its leading
comments and doubles every literal `use_bpm N` and `with_bpm N`. What Sonic
Pi measures in beats (sleeps, envelopes and slides, fx phases and decays,
`duration:`, `beat_stretch:`, `at`, `time_warp`, `sample_duration`, `rt` and
`bt`) must follow the tempo, and what it measures in seconds (horizons,
`kill_delay:`) must not; the oracle says which. With `--check` the script
fails when a copy has drifted from its spec.

## Where the runtime differs on purpose

A spec whose leading comments include `# expected: by hand` pins behaviour
the runtime means to have and Sonic Pi does not (`specs/tau/`: live loops
moving between `with_fx` blocks). Its expected trace is written by hand and
checked by reading it; `gen-expected.rb` never overwrites it, and `check.rb`
skips it when the adapter is the oracle.

`SAMPLES_DIR` is defined for every program: the folder holding Sonic Pi's
built-in samples. A runtime under test must define it too, pointing at a copy
of `oracle/sonic-pi/etc/samples`.

## Horizons

A program that never ends on its own, like a live_loop, can still be a
spec: a line `# horizon: N` among the leading comments stops every thread
whose sleep takes it past N seconds of logical time. The oracle harness and
the runtime (traced and live) honour it alike, so both stop in the same
place. Choose N off the program's beat grid, 2.001 rather than 2, so float
rounding can never decide whether a thread stops on the boundary.

## The app's examples

`specs/examples/` is the app's examples (`web/examples.js`) as specs, each
with a horizon, written by `node scripts/gen-example-specs.mjs`. With
`--check` it fails when a spec has drifted from its example, so an example
that stops matching Sonic Pi is caught like any other spec.

## Ties that float rounding decides

Sonic Pi keeps logical time as a float. When one thread syncs at the very
instant another cues, and the two reached that instant by different chains
of sleeps (one beat against four quarter beats, at 130 bpm), the last bit of
the two sums decides which sorts first, and so whether the sync gets that
cue or waits for the next one. In Sonic Pi that bit depends on the wall
clock the run started at, so such a program is not reproducible there
either. Specs avoid these ties: sync once (`live_loop :x, sync: :cue`), or
reach the instant by the same chain of sleeps as the cue.

