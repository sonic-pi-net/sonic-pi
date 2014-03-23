def drums
  6.times do
    sample :drum_heavy_kick, :rate, 0.8
    sleep 0.5
  end

  8.times do
    sample :drum_heavy_kick, :rate, 0.8
    sleep 0.125
  end
end

def snare
  sample :drum_snare_soft
  sleep 1
end

def synths
  use_synth "saw_beep"
  use_synth_defaults :amp, 0.5, :attack, 0.01, :release, 0.75, :cutoff, 130
  notes = [:F, :C, :D, :D, :G, :C, :D, :D]
  notes.each do |n|
    2.times do
      play note(n, 1)
      play note(n, 2)
      sleep 0.25

      play note(n, 2)
      play note(n, 3)
      sleep 0.25
    end
  end
end

in_thread(:synths) do
  sleep 6
  loop{synths}
end

in_thread(:drums) do
  loop{drums}
end

in_thread(:snare) do
  sleep 12.5
  loop{snare}
end
