load_sample :drum_heavy_kick

in_thread do
  16.times do
    sample :drum_heavy_kick, rate: 0.75
    sleep 0.5
    sample :drum_heavy_kick
    sleep 0.5
  end
end

64.times do
  use_synth :tb303
  play chord(:e3, :minor).choose, attack: 0, release: 0.2, cutoff: rrand(50, 90)
  sleep 0.125
end

32.times do
  use_synth :tb303
  play chord(:a3, :minor).choose, attack: 0, release: 0.2, cutoff: rrand(90, 120)
  sleep 0.125
end

with_fx :reverb do
  32.times do
    use_synth :tb303
    play chord(:e3, :minor).choose, attack: 0, release: 0.2, cutoff: rrand(110, 130)
    sleep 0.125
  end
end

with_fx :echo, phase: 0.25 do
  32.times do
    use_synth :tb303
    play chord(:e3, :minor).choose, attack: 0, release: 0.2, cutoff: rrand(50, 100)
    sleep 0.125
  end
end
