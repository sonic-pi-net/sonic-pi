// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
/** The pre-saved situations: each is a whole Sonic Pi program the page can load. */
export const EXAMPLES = [
  { name: "Two loops in sync", src: `use_bpm 120
use_synth :saw

live_loop :melody do
  play choose([:e3, :g3, :b3, :d4]), amp: 0.5, release: 0.4, cutoff: 90
  sleep 0.5
end

live_loop :bass do
  sync :melody
  use_synth :tb303
  play :e2, release: 0.9, cutoff: 70, res: 0.3
  sleep 1
end
` },
  { name: "Ring of notes, ticked", src: `use_bpm 100
use_synth :pluck

live_loop :arp do
  play [:c4, :e4, :g4, :b4, :c5, :b4, :g4, :e4].tick, amp: 0.5, release: 0.3
  sleep 0.25
end

live_loop :drone do
  use_synth :organ_tonewheel
  play :c3, attack: 2, sustain: 2, release: 2, amp: 0.2
  sleep 6
end
` },
  { name: "Amen, sliced", src: `use_bpm 130

live_loop :amen do
  sample :loop_amen, onset: [0, 1, 2, 3, 4, 5, 6, 7].tick, amp: 0.8
  sleep 0.125
end

live_loop :kick do
  sample :bd_haus, lpf: 70
  sleep 0.5
end
` },
  { name: "Cue and chance", src: `use_bpm 130

live_loop :kick do
  sample :drum_heavy_kick
  cue :beat
  sleep 1
end

live_loop :hat, sync: :beat do
  4.times do
    sample :drum_cymbal_closed, amp: 0.4
    sleep 0.25
  end
end

live_loop :sparkle do
  sync :beat
  use_synth :pretty_bell
  if one_in(3)
    play rrand_i(72, 84), release: 1, amp: 0.4
  end
  sleep 1
end
` },
  { name: "Define and threads", src: `use_bpm 90

define :phrase do |root|
  play_pattern_timed scale(root, :minor_pentatonic).take(5), 0.25, release: 0.2
end

in_thread do
  use_synth :piano
  phrase :e3
  sleep 1
  phrase :a3
end

in_thread do
  use_synth :dark_ambience
  play :e2, sustain: 4, release: 2, amp: 0.5
end
` },
];
