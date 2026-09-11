# Piano wavetable asset

`piano_wavetable.dat` is the sample table for the `MdaPiano` UGen (the `:piano`
synth in Sonic Pi).

It used to be compiled into the engine binary as the `short pianoData[]` array
in `mdaPianoData.h` (~1.1 MB of PCM), which was too big for embedded / WASM
builds, so it became an asset that the engine repository shipped. As of the
clockwork rework the engine no longer carries it at all: `MdaPiano` is handed a
table at runtime through `/supersonic/piano/wavetable <bufnum>`
(`dsp/scsynth/piano_wavetable.h`), and where that table comes from is the
CLIENT's business. So it lives here, in Sonic Pi's tree, rather than in
`external/supersonic`.

## Format

Raw little-endian `int16`, no header: 586349 samples = 1,172,698 bytes. This is
exactly the old `pianoData[]` array dumped in order. The `MdaPiano` UGen indexes
it directly using the keygroup offsets in `mdaPiano_sc3.h`.

## Loading

`app/external/CMakeLists.txt` copies this file to `app/server/native/` beside
the engine binary, which is where `SonicPi::Paths.piano_wavetable_path` looks.
At boot `Studio#load_piano_wavetable` gives it a WAV header once (into the
user's cache directory, since the app's own tree may be read-only), loads that
as a buffer with `/b_allocRead`, points the plugin at it with
`/supersonic/piano/wavetable`, and frees the buffer — the plugin keeps its own
copy.

If the asset is absent, the studio says so and `:piano` plays silent rather
than the engine reading a null table.

## Provenance / license

Derived from Paul Kellett's mda Piano VST, ported to SC3 by Dan Stowell. The mda
plug-ins are released under the MIT license or the GPL (v2 or later). See the
header comment in the engine's `MdaUGens.cpp`.
