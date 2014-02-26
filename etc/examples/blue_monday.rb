def drums
  6.times do
    sample :heavy_kick, :rate, 0.8
    sleep 0.5
  end

  8.times do
    sample :heavy_kick, :rate, 0.8
    sleep 0.125
  end
end

def snare
  sample :snare_soft
  sleep 1
end

def synths
  with_synth "saw_beep"
  with_synth_defaults :amp, 0.5, :attack, 0.01, :release, 0.75, :cutoff, 130
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

in_thread do
  sleep 6
  loop{synths}
end

in_thread do
  loop{drums}
end

in_thread do
  sleep 12.5
  loop{snare}
end
