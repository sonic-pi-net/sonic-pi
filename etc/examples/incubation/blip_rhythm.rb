load_samples [:drum_heavy_kick, :elec_plip, :elec_blip]
use_bpm 100

with_fx :reverb do
  loop do
    with_fx [:echo, :none].choose, room: 0.8, phase: 0.25, mix: 0.8 do
      sample :drum_heavy_kick, amp: 0.5
      sample :elec_plip, rate: [0.5, 2, 1, 4].choose * [1, 2, 3, 10].choose, amp: 0.6
      sample :elec_blip, rate: [0.5, 1, 2].choose, amp: 0.8
      sleep 0.5
    end
  end
end
