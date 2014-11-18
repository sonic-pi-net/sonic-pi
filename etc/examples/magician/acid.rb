use_debug false
load_sample :bd_fat

loop do
  in_thread do
    32.times do
      sample :bd_fat, amp: 2
      sleep 0.5
    end
  end

 sleep 4
  cue :foo
  4.times do |i|
    use_random_seed 667
    16.times do
      use_synth :tb303
      play chord(:e3, :minor).choose, attack: 0, release: 0.1, cutoff: rrand_i(50, 90) + i * 10
      sleep 0.125
    end
  end
  cue :bar
  32.times do
    use_synth :tb303
    play chord(:a3, :minor).choose, attack: 0, release: 0.1, cutoff: rrand_i(90, 130)
    sleep 0.125
  end

  cue :baz
  with_fx :reverb do
    32.times do
      use_synth :tb303
      play chord(:e3, :minor).choose, attack: 0, release: 0.2, cutoff: rrand_i(110, 130)
      sleep 0.125
    end
  end

  in_thread do
    with_fx :echo, phase: 0.25 do
      32.times do
        use_synth :tb303
        play chord(:e3, :minor).choose, attack: 0, release: 0.1, cutoff: rrand(50, 100)
        sleep 0.125
      end
    end
  end

  sleep 16 * 0.125
end
