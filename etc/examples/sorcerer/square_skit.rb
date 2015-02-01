# Coded by Sam Aaron

use_debug false

live_loop :skit do
  with_fx :slicer, phase: 1, invert_wave: 0, wave: 0 do
    with_fx :slicer, invert_wave: 1, wave: 0, phase: 0.25 do
      sample :loop_mika, rate: 1, amp: 2
    end
    sleep 8
  end
end

live_loop :foo, auto_cue: false do |idx|
  #idx = 0
  use_synth :square
  density 2 do
    play (knit :c2, 2, :e1, 1, :f3, 1)[idx], release: 0, attack: 0.25, amp: 1, cutoff: rrand_i(70, 130)
    sleep 0.5
  end
  idx += 0.25
end

live_loop :kik, auto_cue: false do
  density 1 do
    sample :bd_haus, amp: 2
    sleep 0.5
  end
end

live_loop :piano, auto_cue: false do
  sleep 4
  with_fx :slicer, phase: 0.25, wave: 1, invert_wave: true do
    sleep 4
    sample :ambi_piano, amp: 2
  end
end
