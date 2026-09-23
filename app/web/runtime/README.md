# The runtime

The new Sonic Pi language runtime, for mruby. Nothing here is Sonic Pi's
code; every piece is written from the behaviour the oracle pins and judged
by the specs. Written in the Ruby that mruby runs: core classes, no Thread,
and as little of the stdlib as will do.

What exists:

* `lib/sonic_pi/scheduler.rb` — Sonic Pi's threads without threads. Every
  thread is a fiber with its own logical clock; the scheduler runs whichever
  is due next, and never sleeps itself. Two modes over one queue: NRT
  (`run`) resumes the next due thread at once, so a program traces in an
  instant, up to a horizon for one that never ends; RT (`start_job` and
  `step`) resumes a thread when the host's clock reaches its wake time less
  its schedule-ahead, and `step` says when to call again. Thread paths, the
  way a spawned thread runs before its parent carries on, and the (time,
  priority, path, delta) order of cues and syncs all follow Sonic Pi. In RT
  every record leaves through a sink the moment it is made.
* `lib/sonic_pi/lang.rb` — the verbs: sleep, the tempo family, density,
  time_warp, at, in_thread, live_loop, loop, stop, cue, sync, tick, define,
  puts; play, synth, chords and patterns with the whole opts pipeline
  (defaults, transpose, tempo scaling, slides, arg checks); with_fx as far
  as the trace sees it.
* `lib/sonic_pi/samples.rb` — samples as the language sees them: which
  files exist and what is in them (the host says; nothing here opens a
  file), Sonic Pi's rules for names, folders, filters and indexes, onsets
  and slices. The sample verbs are in `lang.rb`, with the whole opts
  pipeline: players, rate from beat_stretch, pitch_stretch and rpitch,
  start and finish from onsets and slices, sample_duration, the defaults
  and sample-bpm families.
* `lib/sonic_pi/note.rb`, `theory.rb` — note names, chords, scales.
* `data/synths.rb`, `data/theory.rb` — generated from the oracle by
  `scripts/gen-synth-data.rb`: every synth's server name, defaults, tempo-
  scaled and slide args, opt aliases and validations; the chord and scale
  tables. `data/samples.rb` (`scripts/gen-sample-data.rb`) has the
  built-in sample groups and, for now, every built-in sample's onsets as
  Sonic Pi's aubio_onset finds them. That table is a stopgap: the runtime
  is meant to find onsets itself, with aubio compiled alongside it, and
  the table is what that must reproduce.
* `lib/sonic_pi/rand.rb` — the random system. Sonic Pi has no generator:
  five tables of 441,000 numbers (one per noise colour, the 16-bit wav files
  under `etc/buffers`), and per thread a seed and an index. The tables are
  kept as bytes and read on demand. A child thread's seed is a draw from its
  parent's stream at the parent's child counter, plus the parent's seed;
  shuffle reseeds from the stream, swaps pairs, and returns to the old
  stream one step on. The current stream is `SonicPi::Rand.current`, which
  the scheduler sets as it switches processes.
* `lib/sonic_pi/rand_verbs.rb` — rand, rand_i, rrand, rrand_i, rdist, dice,
  one_in, choose, pick, shuffle, rand_look, rand_i_look, rand_back,
  rand_skip, rand_reset, use/with_random_seed, use/with_random_source.
* `lib/sonic_pi/float_format.rb` — Ruby prints a Float as the shortest
  decimal that reads back the same; mruby prints 15 digits. Programs see
  floats through `puts`, so this formats them Ruby's way. Checked against
  CRuby on 400,000 values.
* `lib/sonic_pi/ring.rb` — the least of a ring, and what `puts` shows.
* `lib/sonic_pi/adapter.rb` — the runtime as an adapter: runs a program
  and gives its trace as JSON (NRT), and `Live`, a session of jobs ticked
  by the host (RT). `bin/trace.rb` is the trace as a command under an
  interpreter; `bin/trace-wasm.mjs` the same through the wasm under Node;
  `bin/live-check.mjs` drives RT mode through the wasm with a simulated
  clock and compares what it emits with every spec's recording, so the two
  modes are held to the same answer.
* `mruby/` — mruby as a shallow submodule, pinned to a commit of its master
  on the way to 4.1 (c17ead2, 2026-09-15), unpatched; its compiler is Prism,
  which the build fetches as mruby's own submodule.
  `build_config.rb` builds it twice from one gem set, natively and with
  Emscripten. `mrbgems/sonic-pi-core` is the runtime's own C: exact float
  printing, since mruby's is not.
* `host/sp_host.c` — the seam between the wasm and its host: boot, hand a
  random table over, trace a program. The runtime's Ruby is linked in as
  bytecode; nothing is parsed at boot but the program.

```
scripts/build-runtime.sh                                                   # needs rake + emcc; ~1 min
ADAPTER="node runtime/bin/trace-wasm.mjs"                ruby scripts/check.rb specs/random   # the wasm
ADAPTER="build/mruby/host/bin/mruby runtime/bin/trace.rb" ruby scripts/check.rb specs/random   # our mruby
ADAPTER="ruby runtime/bin/trace.rb"                      ruby scripts/check.rb specs/random   # CRuby
```

The wasm is 950 KB, built with 64-bit integers (unboxed, as wasm32
requires) and native wasm exceptions for mruby's longjmp, so it needs a
2022-or-later browser or Node 17+.

All 189 specs pass under CRuby, log tier included. Under our mruby and
the wasm it is 188: the one left uses a Regexp literal, and mruby 4.0 has
no Regexp (4.1 will).

The host's side of the seam, in `host/sp_host.c`: boot, hand over a random
table, name the built-in sample folder, describe a sound file (frames,
channels, rate, onsets), trace a program; and for RT, boot a live session,
run a program as a job at a time on the host's clock, tick, stop all, with
every record arriving through `sp_emit` as JSON. The page drives that
against SuperSonic's clock: `Run live` in the playground plays a program
until Stop, a new Run takes over running live_loops by name, and output
and errors arrive as they happen. The tick loop runs in a worker of its own
(`web/live-worker.js`), so the page's main thread never delays a sound.

Where the interpreters had to be reconciled: mruby lays out a float from
1e15 to 1e16 in exponent form where CRuby does not, and rounds with
`Float#round(digits)` differently near 2**52 (the C helper and
`float_format.rb`),
and reports a bare unknown name as NoMethodError where Ruby says NameError
(the language's method_missing says one thing for both, and the oracle's
trace is normalised to it).

A small mruby build refuses arrays of 441,000 elements, which the code
avoids. The pin is master rather than 4.1.0-rc2 because the release
candidate compiles an eval string against scopes past a method's boundary:
a method the runtime's source defines by eval inside a block could not
later run a program naming one of that block's locals ("Can't find local
variables"). Master has the fix (mruby fecbc01bb).

## In the browser

`web/sonic_pi.js` is the runtime's host in a page. `loadRuntime` fetches
the wasm, installs the random tables and describes the built-in samples.
`bootEngine` starts SuperSonic from the CDN. `Bridge` turns records into
bundles, loads each synthdef and sample once, and preloads what a program
names before it runs, so a first hit is not late. `LiveSession` is the tick
loop against SuperSonic's clock; `sp_live_status` tells it which runs and
named threads are alive, for the page to show. The app (`web/index.html`)
and the spec browser (`web/specs.html`) are both built on it.
`sp_live_stop_after` gives a live session a spec's horizon, for
`bin/live-check.mjs`; the app never sets one.

