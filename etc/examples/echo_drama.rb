use_synth :tb303

rate = 0.7
rate_mul = 1.0 / rate

with_fx(:echo, delay: 0.5 * rate_mul, decay: 4) do
  loop do
    play chord([:b1, :b2, :e1, :e2, :b3, :e3].choose, :minor).choose, cutoff: rrand(40, 100), amp: 0.5, attack: 0, release: 2
    sleep [0.25, 0.5, 0.5, 0.5, 1, 1].choose * rate_mul
  end
end
