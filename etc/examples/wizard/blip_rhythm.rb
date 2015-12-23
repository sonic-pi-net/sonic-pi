# Coded by Sam Aaron

load_samples [:drum_heavy_kick, :elec_plip, :elec_blip]
use_bpm 100
use_random_seed 100

with_fx :reverb, mix: 0.6, room: 0.8 do
  with_fx :echo, room: 0.8, decay: 8, phase: 1, mix: 0.4 do
    live_loop :blip do
      n = [:e2, :e2, :a3].choose

      with_synth :dsaw do
        with_transpose -12 do
          in_thread do
            2.times do
              play n, attack: 0.6, release: 0.8, detune: rrand(0, 0.1), cutoff: rrand(80, 120)
              sleep 3
            end
          end
        end
      end

      sleep 4

      with_synth :tri do
        play chord(n, :m7), amp: 5, release: 0.8
      end

      sleep 2
    end
  end
end


with_fx :echo, room: 0.8, decay: 8, phase: 0.25, mix: 0.4 do
  live_loop :rhythm do
    sample :drum_heavy_kick, amp: 0.5
    sample :elec_plip, rate: [0.5, 2, 1, 4].choose * [1, 2, 3, 10].choose, amp: 0.6
    sleep 2
  end
end
