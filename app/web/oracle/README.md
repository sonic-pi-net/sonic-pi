# The oracle

`sonic-pi/` is the Sonic Pi repository, a shallow submodule pinned to a
release tag. The runtime is `app/server/ruby`; it finds `etc/` (samples,
synthdefs, the random tables under `etc/buffers`) relative to itself, which
is why the whole repository is here and not a copy of the guts. Its gems are
vendored under `app/server/ruby/vendor`. Its nested submodules (the audio
engine and friends) are never needed.

`bin/aubio_onset` is the one native piece: the small program Sonic Pi runs
to find a sample's onsets, built by `scripts/build-oracle.sh` from the aubio
source and front-end Sonic Pi carries in its tree. The harness points the
runtime at it, so the submodule stays untouched.

`harness/oracle.rb` builds the language the way Sonic Pi's test suite does
(the runtime modules mixed into one object) but with a recording studio in
place of `SonicPi::Studio`: every synth trigger and sample load is recorded
with the thread's logical time, beat and spawn path; nothing is sent
anywhere. Programs run as real Sonic Pi jobs, so `sleep` really sleeps and
threads are real threads; the trace records logical time only, which is why
it is the same from run to run. The Link stand-in is a steady 60 bpm
timeline whose beat 0 is the run's start. Sample metadata (frames, channels,
rate) is read from the FLAC or WAV header, so no decoder or tool is needed.

What the trace contains and how specs use it: `../specs/README.md`.

The harness starts the wall clock near zero. Sonic Pi takes logical time
from `Time.now.to_f`, about 1.8e9 seconds, where a double cannot hold a
microsecond, so every sleep rounds away a quarter of one and a trace drifts
by a millionth of a second every few beats. The runtime only subtracts and
compares clock readings, so a clock that counts from just before the harness
started changes nothing but that noise.

