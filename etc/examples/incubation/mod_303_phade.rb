use_synth :tb303

live_loop :foo do
  sleep 0.5
end

sync :foo

with_fx :reverb do
  with_fx :slicer, phase: 0.5, wave: 0 do
    play 50 - 24, cutoff: 120, cutoff_attack: 0.3, res: 0.07, release: 60
  end
end
