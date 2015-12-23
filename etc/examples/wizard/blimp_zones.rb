# Coded by Sam Aaron

use_debug false
use_random_seed 667
load_sample :ambi_lunar_land
sleep 1

live_loop :foo do
  with_fx :reverb, kill_delay: 0.2, room: 0.3 do
    4.times do
      use_random_seed 4000
      8.times do
        sleep 0.25
        play chord(:e3, :m7).choose, release: 0.1, pan: rrand(-1, 1, res: 0.9), amp: 1
      end
    end
  end
end

live_loop :bar, auto_cue: false do
  if rand < 0.25
    sample :ambi_lunar_land
    puts :comet_landing
  end
  sleep 8
end

live_loop :baz, auto_cue: false do
  tick
  sleep 0.25
  cue :beat, count: look
  sample :bd_haus, amp: factor?(look, 8) ? 3 : 2
  sleep 0.25
  use_synth :fm
  play :e2, release: 1, amp: 1 if factor?(look, 4)
  synth :noise, release: 0.051, amp: 0.5
end
