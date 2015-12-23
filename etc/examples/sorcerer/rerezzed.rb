# Coded by Sam Aaron

use_debug false
notes = (scale :e1, :minor_pentatonic, num_octaves: 2).shuffle

live_loop :rerezzed do
  tick_reset
  t = 0.04
  sleep -t
  with_fx :bitcrusher do
    s = synth :dsaw, note: :e3, sustain: 8, note_slide: t, release: 0
    64.times do
      sleep 0.125
      control s, note: notes.tick
    end
  end
  sleep t
end

live_loop :industry do
  sample :loop_industrial, beat_stretch: 1
  sleep 1
end

live_loop :drive do
  sample :bd_haus, amp: 3
  sleep 0.5
end
