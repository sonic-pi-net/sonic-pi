define :foo do
  sync :tick
  with_fx :echo do
    with_fx :lpf, cutoff: 100 do
      sample :ambi_choir, rate: 1 * [0.125, 0.25, 0.5, 1].choose
    end
  end
  in_thread do
    sample :drum_heavy_kick, amp: 2
    sleep 0.5
    sample :elec_blip
  end
  in_thread do
    with_fx :distortion, distort: 0.1 do
    with_fx :rlpf do |e|
      4.times do
        control e, cutoff: rrand(50, 80)
        sample :drum_heavy_kick, rate: 1
        sleep 0.25
      end
    end
  end

  end
  with_fx :echo do
    sample :elec_blip, rate: 0.5
  end
end

in_thread do
  loop {foo}
end

loop do
  cue :tick
  sleep 1
end
