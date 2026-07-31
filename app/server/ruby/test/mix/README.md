# Mix measurement harness

Measures what the main mixer actually does to audio, so changes to the master
chain can be shown to help rather than argued about.

Every number comes from a real render: a headless SuperSonic plays an exact
test signal through the real `sonic-pi-mixer` synthdef and the output is
captured sample-accurately with `RecordBuf` and written to disk. No audio
device is involved and nothing is modelled or simulated.

## Running

    ruby app/server/ruby/test/mix/mix_report.rb                    # measure the shipping mixer
    ruby app/server/ruby/test/mix/mix_report.rb --mixer other.scsyndef   # A/B a candidate chain
    ruby app/server/ruby/test/mix/test_mix_quality.rb              # regression tests

The harness synthdefs are built once with `synthdefs/build.sh`, which needs
SuperCollider's `sclang` (same requirement as the shipping synthdefs in
`etc/synthdefs/designs`). The compiled results are checked in, so the tests
run without SuperCollider installed. They skip if the engine binary is
missing.

## What it measures

| measure | what it tells you |
| --- | --- |
| sample peak, true peak | true peak is 4x oversampled per BS.1770-4, so inter-sample overs a sample-peak meter calls legal are visible |
| RMS, LUFS-I, LUFS-S | programme loudness with R128 gating: the number that says how loud something really is |
| crest factor | peak minus RMS: how much of the level is transient and how squashed the mix is |
| gain reduction | measured against a limiter-bypassed render of the same signal, not inferred from how far the input went over |
| stereo link error | how far apart the gains applied to L and R drift, i.e. how much the image moves when the limiter works |
| THD+N, IMD | distortion, and the intermodulation products that are the measurable form of "mud" |
| DC offset | LeakDC regressions |

Test signals live in `lib/mix_signals.rb` and are analytic or seeded, so a
measurement that moves means the mixer moved, not the signal.

## Gotchas worth knowing

These each cost a debugging session; they are baked into the harness now.

- **Control values must be Floats.** scsynth reads an integer where a float is
  expected as a different value, and a synth with a silently wrong buffer
  number renders silence rather than failing.
- **Skip the settle window.** `pre_amp` and `amp` are lagged by 20 ms and the
  safety filters ring on startup. Measuring from sample zero reports about 10%
  THD on a path that is actually transparent to -143 dB.
- **Start every node in one bundle.** Sent as separate messages they can land
  in different control blocks, and two renders that should be identical end up
  offset.
- **Never time a render by wall clock.** Headless paces itself with a real-time
  timer and drops blocks when it falls behind, so a busy machine renders less
  than a second of audio per second and the capture is cut short. The silent
  tail then measures as -Infinity, and two renders truncated at different
  points stop lining up. Wait for the source node's `/n_end` instead: that
  tracks rendered time however slowly it arrives.
- **Align before comparing two renders.** SC's `Limiter` delays the signal, so
  a limiter-bypassed reference arrives early; comparing without compensating
  measures the offset rather than the gain.
- **Coherent measures need whole cycles.** A single-bin DFT over a
  non-integer number of cycles leaks about 42 dB, which buries every
  distortion product underneath it. `whole_seconds` trims to a whole number of
  seconds, which is exact for integer-Hz test signals.
- **Gate quiet blocks out of gain ratios.** A beat null divides two near-zero
  numbers and reports tens of dB of gain change that nothing audible
  corresponds to.
