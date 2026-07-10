# Coded by Sam Aaron

live_loop :time do
  synth :tb303, release: 8, note: :e1, cutoff: (range 90, 60, -10).tick, amp: 0.8
  sleep 8
end

live_loop :machine do
  sample :loop_garzul, rate: (knit 1, 3, -1, 1).tick, hpf: 80
  sleep 8
end

live_loop :vortex, auto_cue: false do
  use_synth [:pulse, :beep].choose
  with_fx :hpf, cutoff: 42, amp: 0.8 do
    play scale(:e1, :minor_pentatonic).tick, release: 0.1, amp: 2, cutoff: (ring 70, 90, 100, 130).choose
  end
  sleep 0.125
end

live_loop :moon_bass, auto_cue: false do
  sample :bd_haus, amp: 1.5, lpf: 110
  sleep 0.5
end
