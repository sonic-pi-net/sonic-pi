load_samples [:drum_heavy_kick, :drum_snare_soft]

define :drums do
  6.times do
    sample :drum_heavy_kick, rate: 0.8
    sleep 0.5
  end

  8.times do
    sample :drum_heavy_kick, rate: 0.8
    sleep 0.125
  end
end

define :snare do
  sample :drum_snare_soft
  sleep 1
end

define :synths do
  use_synth :mod_saw
  use_synth_defaults amp: 0.5, attack: 0, sustain: 1, release: 0.25, cutoff: 90, mod_range: 12, mod_phase: 0.5
  notes = [:F, :C, :D, :D, :G, :C, :D, :D]
  notes.each do |n|
    play note(n, octave: 1)
    play note(n, octave: 2)
    sleep 1
  end
end

in_thread(name: :synths) do
  sleep 6
  loop{synths}
end

in_thread(name: :drums) do
  loop{drums}
end

in_thread(name: :snare) do
  sleep 12.5
  loop{snare}
end
