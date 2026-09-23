# Sonic Pi on the web

This is Sonic Pi running in a browser. The language runs on mruby compiled to
WebAssembly, and the sound comes from SuperSonic, which is scsynth on
clockwork. There is no server: the page does all of it.

The same runtime builds as a native command too, and the plan is for it to
replace the Ruby server in the app. So the point of everything here is that
your Sonic Pi programs do the same thing wherever they run.

## How we know it does the same thing

The Ruby server in this repository is the oracle. A spec is a small program.
We run it through the server, record what it did, and commit that recording.
A runtime passes the spec when it does the same.

A recording is the OSC the engine is sent: which synth, which opts, at which
moment. If two runtimes send the same OSC, they make the same sound.

There are over 500 specs, covering threads, time, samples, fx, the random system,
ticks, MIDI, tempo and whole tracks. They run in real time in the oracle, so
keep a new one under about two seconds.

The rules an opt has to keep live in one file, the server's own
`validation.rb`. The server reads it, and the runtime is built from it, so
`cutoff: 200` is refused the same way in both.

## Building it

See [BUILD-WEB.md](../../BUILD-WEB.md) for the whole thing. The short version:

```
git submodule update --init --depth 1 app/web/runtime/mruby
npm ci
scripts/build-runtime.sh        # mruby, then the runtime as wasm and as a command
node scripts/build-web.mjs      # build/web: static files, ready to host
```

## Running the checks

```
scripts/build-oracle.sh                             # aubio_onset, from Sonic Pi's own sources
ruby scripts/check.rb                               # rerun the server: are the recordings still what it does?
ADAPTER="$PWD/build/runtime/sp-trace" ruby scripts/check.rb          # the specs, through mruby itself
ADAPTER="node $PWD/runtime/bin/trace-wasm.mjs" ruby scripts/check.rb # through the module the page loads
ruby scripts/check-validations.rb                   # every opt's rule, against the server's own checking
ruby scripts/check-corpus.rb                        # every example and tutorial snippet, with Safe mode on
node runtime/bin/live-check.mjs                     # a live session, and the OSC it sends
node scripts/browser-check.mjs                      # the pages themselves, in Chromium and WebKit
node scripts/share-check.mjs                        # every shipped program through the share link and back
```

CI runs all of these ([.github/workflows/web.yml](../../.github/workflows/web.yml)),
and the specs on Linux, macOS and Windows.

An adapter is any command that takes a program and prints a recording, so a
new runtime can be held to the same specs:

```
ADAPTER="<your runtime>" ruby scripts/check.rb
ADAPTER="<your runtime>" node scripts/serve.mjs     # Evaluate and Run use it too
```

## Working on it

```
node scripts/serve.mjs                       # 127.0.0.1:8460/web/ the app, /web/specs.html the specs
node scripts/build-app.mjs                   # after an edit to app/src
scripts/build-runtime.sh --wasm              # after an edit to the runtime's Ruby
scripts/build-runtime.sh --native            # the same runtime as a command, no Emscripten needed
ruby scripts/gen-expected.rb [specs/play]    # rewrite recordings after the server's behaviour changes
ruby scripts/gen-synth-data.rb               # the synths, their opts and their rules, from the server
ruby scripts/gen-editor-data.rb              # what the editor knows: completions, reference, cards
node scripts/gen-example-specs.mjs           # the app's examples as specs
node scripts/gen-shortcuts.mjs               # the keyboard shortcuts, read from the GUI's source
```

Two specs are marked `# racy:`. A sample played before it has finished
loading comes from the loader's thread rather than the calling one, so the
server can record the same program two ways. `check.rb` runs those again
before believing a failure, and only when it is the server being rerun.

## What is where

* `app/src/` — the app: editor, docs, quickstart cards, error card, scope,
  piano roll, shortcuts.
* `runtime/` — the runtime's Ruby, the C that hosts it, and the generated
  data it needs. `runtime/bin/` has the harnesses.
* `specs/` — a program each, with the server's recording beside it.
  `specs/README.md` has the format.
* `oracle/harness/oracle.rb` — runs the server against a recording studio
  instead of a synth server, and prints what it did.
* `web/` — what is served. `index.html`, `examples.html`, `learn.html`,
  `support.html` and the tutorial's pages are sonic-pi.net's pages, each the app
  with that page in it, and `code.html` is the editor (`scripts/build-site.mjs`); `specs.html` is the spec browser,
  where you can play a spec, run it on mruby and compare.
* `site/` — the pages' own source: `pages/*.html` (plain HTML, a `{{…}}` where
  the build fills something in), `partials/`, `data/`, `css/` and `media/`.
  A page reads with no script at all; the app's script adds the live cards and
  goes from page to page without reloading. The Tutorial comes from `etc/doc`
  instead, through the app's own `qt-doc.rb` and `web-tutorial.rb`.
* `scripts/` — the builders, the generators and the checks above.

## The sound

SuperSonic is served as `/web/supersonic/`. By default it comes from its
released packages on the CDN, at the version `package.json` pins
(`supersonicVersion`), so a host serves none of the engine and a deploy needs
no engine build. While the engine itself is being worked on,
`--supersonic local` uses the `dist/` that `scripts/build-web.sh` writes in
`app/external/supersonic`, and `--assets cdn` leaves the synthdefs and
samples (35 MB that rarely change) on the CDN.

SuperSonic is AGPL. If you hand someone a build with the engine in it,
publish the source you built it from.

## Sharing

A share link is the program itself. `#code=`, then a format character, then
the program compressed: its tokens from a fixed table, deflated against a
dictionary of the language's common lines (`app/src/share.js`). Nothing is
stored anywhere.

## Licence

This directory is AGPL-3.0-or-later, the same as the engine it plays through. The repository's
[LICENSE.md](../../LICENSE.md) has the detail, including what it serves that is licensed
otherwise.
