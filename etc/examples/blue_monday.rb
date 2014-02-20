def drums
  6.times do
    sample :heavy_kick, :rate, 0.8
    sleep 0.5
  end

  8.times do
    sample :heavy_kick, :rate, 0.8
    sleep 0.125
  end
  #sample :glass_hum, :rate, 0.5
end

def snare
  sample :snare_soft
  sleep 1
end

def synths
  with_synth "saw_beep"
  notes = [:F, :C, :D, :D, :G, :C, :D, :D]
  notes.each do |n|
    2.times do
      play note(n, 1), :amp, 0.5, :attack, 0.01, :release, 0.5
      play note(n, 2), :amp, 0.5, :attack, 0.01, :release, 0.75

      sleep 0.25
      play note(n, 2), :amp, 0.5, :attack, 0.01, :release, 0.5
      play note(n, 3), :amp, 0.5, :attack, 0.01, :release, 0.75

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
