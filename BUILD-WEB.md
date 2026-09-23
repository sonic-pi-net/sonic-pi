# Building Sonic Pi for the web

This builds Sonic Pi as a set of static files: the editor, the docs, the
tutorial, the examples and the spec browser, with the language running on
mruby compiled to WebAssembly and the sound coming from SuperSonic. Any
static host can serve the result — there is no server side.

Everything it needs is in this repository. Nothing is fetched while it
builds except SuperSonic itself, and there is a flag for that too.

### What you need

- **Ruby 3.x** — the same one the app's server uses. `rake` comes with it.
- **Node 22 or later**, with npm.
- **A C compiler** — clang or gcc.
- **Emscripten 4.0.14** — for the WebAssembly build only. If you just want
  to run the specs, you can skip it (see *Without Emscripten* below).

On macOS the compiler comes with the Xcode command line tools
(`xcode-select --install`). On Debian or Ubuntu: `apt install build-essential
ruby-full nodejs npm`. Emscripten's own instructions are at
https://emscripten.org/docs/getting_started/downloads.html — pin 4.0.14, since
the compiler version is part of what the module is.

### Building it

```bash
git submodule update --init --depth 1 app/web/runtime/mruby

# the tutorial, from etc/doc — 86 sections, every image from this repository
cd app/server/ruby
ruby bin/qt-doc.rb
ruby bin/web-tutorial.rb

cd ../../web
npm ci
scripts/build-runtime.sh        # mruby, then the runtime as wasm and as a command
node scripts/build-web.mjs      # everything, into build/web
```

`build/web` is then about 25 MB and complete: the site's pages (`index.html`
and the rest, each with the app in it), the app, its data and themes,
`specs.html` with every spec, and the
runtime's wasm with the random tables and the facts about the built-in
samples.

### Serving it

The engine shares memory with the page (SharedArrayBuffer) when the host
sends two headers with it:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

Without them the page still plays, through SuperSonic's postMessage mode,
but recording and the scopes that read the engine's memory are not there.
Everything else is ordinary static hosting; `.wasm` should be served as
`application/wasm`.

### Where SuperSonic comes from

By default the build points at SuperSonic's released packages on the CDN, so
a host serves none of the engine's bytes. The version is pinned in
`app/web/package.json` (`supersonicVersion`), so building the same commit
always gives the same engine: moving to a new release is a one-line commit,
tried on a rehearsal first. `--supersonic cdn@<version>` tries another without
changing the pin. The native app's engine is the `app/external/supersonic`
submodule, pinned on its own: the two needn't match. Two flags change where it
comes from:

```bash
node scripts/build-web.mjs --supersonic local   # the local build, for an unreleased engine
node scripts/build-web.mjs --assets local       # the synthdefs and samples too (35 MB)
```

SuperSonic is AGPL: if you distribute a build with the engine in it, publish
the source it was built from. The build prints a reminder when the engine it
found is not on its remote.

### Working on it

```bash
node scripts/serve.mjs                    # https://localhost:8460/web/ — the app, and /web/specs.html
node scripts/serve.mjs --host 0.0.0.0 --https    # reach it from a phone on the same network
```

The dev server serves what is in the tree rather than a build, so an edit to
`app/src` needs `node scripts/build-app.mjs` and an edit to the runtime's Ruby
needs `scripts/build-runtime.sh --wasm`.

The checks, all of which CI runs (`.github/workflows/web.yml`):

```bash
ADAPTER="$PWD/build/runtime/sp-trace" ruby scripts/check.rb   # the specs, through mruby itself
ADAPTER="ruby $PWD/runtime/bin/trace.rb" ruby scripts/check.rb   # through MRI
ADAPTER="node $PWD/runtime/bin/trace-wasm.mjs" ruby scripts/check.rb   # through the module the page loads
ruby scripts/check.rb                     # rerun the server: are the traces still what it does?
ruby scripts/check-validations.rb         # every opt's rule, against the server's own checking
ruby scripts/check-corpus.rb              # every example and tutorial snippet, with Safe mode on
node runtime/bin/live-check.mjs           # a live session, and the OSC it sends
node scripts/browser-check.mjs https://localhost:8460/web/    # the page, in Chromium and WebKit
```

A spec is a trace: which synth, with which opts, at which moment — which is
the OSC the engine is sent. A spec that passes is the same sound.

### Without Emscripten

The runtime also builds as a native command, which is how the specs are run
on machines that will never see a browser:

```bash
scripts/build-runtime.sh --native
ADAPTER="$PWD/build/runtime/sp-trace" ruby scripts/check.rb
```

That builds the host interpreter if it is missing and asks for no Emscripten
at all.

### What is generated, and what is kept

Generated, and committed because the browser cannot generate it:

- `runtime/data/` — every synth and FX, their defaults and the rules their
  opts are held to, from `app/server/ruby` (`scripts/gen-synth-data.rb`).
- `web/data/` — what the editor knows: completions, the reference, the
  quickstart cards (`scripts/gen-editor-data.rb`).
- `specs/**/*.expected.json` — what the server did, which is what a runtime
  is held to (`scripts/gen-expected.rb`).

Change the server's behaviour and these want regenerating; CI fails when
they no longer match, and the diff is the record of what changed.

Built, and not committed: `build/`, `web/app.js`, `web/index.html`,
`web/site/`.

### If something goes wrong

- **`no mruby`** — the submodule is not checked out: `git submodule update
  --init --depth 1 app/web/runtime/mruby`.
- **`emcc: command not found`** — either activate Emscripten's environment
  (`source /path/to/emsdk/emsdk_env.sh`) or build with `--native`.
- **No recording and no scopes, and the console mentions SharedArrayBuffer**
  — the two headers above are missing from the host.
- **There is no Tutorial page** — `ruby bin/qt-doc.rb` and
  `ruby bin/web-tutorial.rb` have not been run, so there is no tutorial to
  show.
