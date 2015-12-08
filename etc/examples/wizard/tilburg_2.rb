# Coded by Sam Aaron

use_debug false
load_samples :guit_em9, :bd_haus

live_loop :low do
  tick
  synth :zawa, wave: 1, phase: 0.25, release: 5, note: (knit :e1, 12, :c1, 4).look, cutoff: (line 60, 120, steps: 6).look
  sleep 4
end

with_fx :reverb, room: 1 do
  live_loop :lands, auto_cue: false do
    use_synth :dsaw
    use_random_seed 310003
    ns = (scale :e2, :minor_pentatonic, num_octaves: 4).take(4)
    16.times do
      play ns.choose, detune: 12, release: 0.1, amp: 2, amp: rand + 0.5, cutoff: rrand(70, 120), amp: 2
      sleep 0.125
    end
  end
end

live_loop :fietsen do
  sleep 0.25
  sample :guit_em9, rate: -1
  sleep 7.75
end

live_loop :tijd do
  sample :bd_haus, amp: 2.5, cutoff: 100
  sleep 0.5
end

live_loop :ind do
  sample :loop_industrial, beat_stretch: 1
  sleep 1
end
