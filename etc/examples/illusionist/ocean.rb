with_fx :reverb, mix: 0.5 do
  loop do
    s = synth [:bnoise, :cnoise, :gnoise].choose, attack: rrand(0, 1), sustain: rrand(0.5, 5), cutoff: rrand(60, 100), pan: rrand(-1, 1), pan_slide: 1, amp: rrand(0.5, 1)
    control s, pan: rrand(-1, 1)
    sleep rrand(2, 4)
  end
end
