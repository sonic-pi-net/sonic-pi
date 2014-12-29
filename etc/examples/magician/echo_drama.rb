# Coded by Sam Aaron

use_random_seed 667

with_fx :lpf, cutoff: 90 do
  with_fx :reverb, room: 0.8, mix: 0.25 do
    with_fx :distortion, distort: 0.3, amp: 0.8 do
      with_fx(:echo, delay: 0.5, decay: 4) do
        live_loop :foo do
          use_bpm 45
          use_synth :tb303
          play chord([:b0, :e0, :b1, :b2, :e1, :e2, :b3, :e3].choose, :minor).choose, cutoff: rrand(40, 100), amp: 0.5, attack: 0, release: 2
          sleep [0.25, 0.5, 0.5, 0.5, 1, 1].choose
        end
      end
    end
  end
end
