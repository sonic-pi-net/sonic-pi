# the app's example "Cue and chance" (web/examples.js), recorded up to its horizon
# horizon: 2.001
use_bpm 130

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
