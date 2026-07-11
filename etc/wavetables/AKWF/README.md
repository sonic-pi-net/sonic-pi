# AKWF Wavetables

Adventure Kid WaveForms by Kristoffer Ekstrand (adventurekid.se), public
domain. 4358 single-cycle waves in 65 families.

Per wave:

- `*.wav` — 1024-sample 16-bit cycle (75264 Hz => 73.5 Hz fundamental at
  unity rate). Stereo sources keep 2 channels.
- `*.wavetable` — AIFF-C float32, 2048 frames: the mono cycle in
  SuperCollider **wavetable format**, loadable directly via `/b_allocRead`
  into `Osc` / `OscN` / `VOsc` / `VOsc3` / `COsc`.

## Provenance

Regenerated July 2026 from the canonical 600-sample AKWF originals
(github.com/KristofferKarlAxelEkstrand/AKWF-FREE, dir `AKWF/`) with
SuperSonic's exact spectral pipeline
(`supersonic/scripts/build-wavetables.mjs --sonic-pi=<dir>`):

- exact DFT-domain resampling (alias-free for periodic cycles; verified
  alias floor −147 dB worst-case vs −17 dB in the previous generation)
- exact DC removal
- fundamental phase-aligned (exact circular rotation; waveform shape and
  spectrum preserved) so adjacent tables morph through VOsc without
  fundamental-phase cancellation
- `.wavetable` written at full float32 precision (no int16 round-trip);
  in-band match to the originals is −158 dB median
- stereo folds guarded against phase cancellation (falls back to L channel;
  affects 1 table: AKWF_stereo_0100)
