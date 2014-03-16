with_synth "prophet"

with_fx(:echo, :delay, 0.5, :decay, 4) do
  loop do
    play chord([:b1, :b2, :e1, :e2, :b3, :e3].choose, :minor).choose, :cutoff, rrand(40, 100), :amp, 0.5
    sleep [0.25, 0.5, 1].choose
  end
end
