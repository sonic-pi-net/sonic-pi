with_fx :reverb do
  in_thread do
    loop do
      r = [0.5, 1.0/3, 3.0/5].choose
      8.times do
        sample :ambi_choir, rate: r, pan: rrand(-1, 1)
        sleep 0.5
      end
    end
  end
end

with_fx :wobble, phase: 2 do |w|
  with_fx :echo, mix: 0.6 do
    loop do
      sample :drum_heavy_kick
      sample :bass_hit_c, rate: 0.8, amp: 0.4
      sleep 1
      #control w, phase: [0.5, 1, 2].choose
    end
  end
end
