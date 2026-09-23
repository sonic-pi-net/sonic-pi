# Plan: the app page, from the Sonic Tau GUI

Make `web/` a Sonic Pi you can play, with the Sonic Tau page's shape, over
the mruby runtime and SuperSonic. The spec browser stays as the test view,
one link away.

Status: superseded by `PLAN-sonic-tau-gui.md`. This plan took its layout
from `next-gen/sonic-tau`, a small experiment that shares the name, not
from Sonic Tau itself (tau-state's app, hosted at sonic-pi.net/tau). What
it built that stays useful: `web/sonic_pi.js`, `web/specs.html`, the spec
horizon, and the examples as specs.

## 1. One shared module: `web/sonic_pi.js`

Today the runtime loading, the live tick loop and the OSC bridge live inline
in the spec page. Pull them into a module both pages import:

* `loadRuntime(base)` — fetch the wasm, install the five random tables and
  the sample facts, return `{ module, version, trace(code) }`.
* `bootEngine()` — SuperSonic from the local build with Sonic Pi's synthdefs
  and samples beside it.
* `Bridge(engine)` — loads each synthdef and sample once, turns a synth
  record into a bundle stamped for its time, plays a whole trace, silences.
* `LiveSession(runtime, engine, handlers)` — `run(code)` starts a job now;
  the tick loop sleeps until the scheduler's next wake time; `stop()` drops
  every job and frees what sounds. Handlers receive output, log and error
  records with a time since the session's first run.

The draft in the stash is the working code from the spec page, moved; it is
the part with the least risk.

## 2. The app page: `web/index.html`

Sonic Tau's layout and styling, with its words changed for what this is.

* **Header.** "Sonic Pi", and a line saying the language runs on mruby in
  the browser and the sound comes from SuperSonic.
* **Program panel.** Boot the engine, Run, Stop, an examples dropdown, the
  keyboard hint (Cmd/Ctrl+Enter runs, Esc stops), the editor, a status
  line. Run is Sonic Pi's Run: a new job, the old ones carry on, live loops
  with the same name take over. Stop is Stop.
* **Aside.** Share link, as in Sonic Tau: the program lives in the URL hash,
  so a link is a situation. Below it the panels Sonic Pi has and Sonic Tau
  did not need: **Output** for `puts`; **Errors** as the red panel with line
  and thread, kept until the next run; the **Engine** log. Then the language
  hint, rewritten for the real verbs: synths by name, `sample` with the
  built-in names and opts, `in_thread`, `live_loop`, `define`, cue and
  sync, tick, the random verbs.
* **Footer.** What the two halves are, and a link to the specs page.

Two additions over Sonic Tau that this runtime makes possible:

* **Trace.** A button that shows the program's whole schedule as a table
  without playing it — the NRT view. It is the spec page's renderer, and
  it is the first thing to reach for when a program is silent: it says
  what would have been sent.
* **Runtime badge.** Which mruby is loaded, and a clear message if the wasm
  is missing from the build.

## 3. Examples: `web/examples.js`

Sonic Tau's four, translated to real Sonic Pi, since its synth names were
Tau's: `:lead` becomes `:saw`, `:organ` becomes `:organ_tonewheel`, the hat
on `:noise` becomes a cymbal sample. Plus one with samples and onsets, and
one with `define`, `in_thread` and `play_pattern_timed`, so every family
the runtime supports has a situation.

Each example is also a candidate spec: run each through the oracle and the
runtime and keep the traces under `specs/examples/`, so the examples cannot
silently stop working.

## 4. The spec browser: `web/specs.html`

Move the current page to `specs.html`, replace its inline runtime and
bridge code with the module, and link it from the app. Evaluate, Play and
the playground stay as they are. This is the largest edit by lines and the
least interesting; the stash has a first pass.

## 5. Build and serve

`scripts/build-web.mjs` copies both pages, the module and the examples; the
dev server needs no change beyond serving them. The static build in
`build/web/` then opens as the app with no server at all, which is the demo
shape.

## 6. Two behaviours to get right

* **Sample preload.** A sample loads on first use, half a second before its
  trigger. A slow CDN fetch can miss the first hit. At Run, scan the program
  for `sample :name` and `SAMPLES_DIR` mentions and start those loads before
  the job starts. A heuristic, but it removes the common case.
* **Run while running.** Verified already in the spec page: a second Run
  redefines live loops in place. The app should show which runs are alive
  and let Stop end them all — Sonic Pi's own model — rather than offer a
  per-run stop.

## 7. Verification

* A Playwright script for the app: load, boot, pick an example, run, see
  output and the count of bundles sent, redefine a loop, stop, share-link
  round trip.
* The existing spec-page script, rerun on `specs.html`.
* All 215 specs still passing through the wasm: the module refactor touches
  nothing in the runtime.

## 8. Order and size

1. Module out of the spec page; spec page on the module; tests green again.
   About an hour.
2. App page and examples. About an hour.
3. Sample preload, the trace button, the examples as specs. Under an hour.
4. README and commit, one commit per step.

## Open choices

* Is the app `index.html` and the specs `specs.html`, as above, or the
  other way round?
* The editor is a textarea for now. The CodeMirror choice from the
  accessibility discussion is a later step and does not change this plan.
* Whether the app shows the scheduler's log lines (the synth echoes) by
  default, or only output and errors. Sonic Pi shows the echoes; the
  default here would be to show them.
