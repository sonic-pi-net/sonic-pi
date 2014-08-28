in_thread do
  loop do
    cue :tick
    sleep 1
  end
end

in_thread do
  loop do
    sync :tick
    sample :drum_heavy_kick
  end
end

in_thread do
  use_synth :mod_saw
  loop do
    sync :tick
    play chord(:e1, :minor).choose, mod_phase: [1, 0.5, 0.25, 0.125].choose, cutoff: rrand(80, 110)
  end
end
