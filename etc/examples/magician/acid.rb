# Coded by Sam Aaron

use_debug false
load_sample :bd_fat

8.times do
  sample :bd_fat, amp: (line 0, 5, steps: 8).tick
  sleep 0.5
end

live_loop :drums do
  sample :bd_fat, amp: 5
  sleep 0.5
end

live_loop :acid do
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
  32.times do |i|
    use_synth :tb303
    play chord(:a3, :minor).choose, attack: 0, release: 0.05, cutoff: rrand_i(70, 98) + i, res: rrand(0.9, 0.95)
    sleep 0.125
  end

  cue :baz
  with_fx :reverb, mix: 0.3 do |r|
    32.times do |m|
      control r, mix: 0.3 + (0.5 * (m.to_f / 32.0)) unless m == 0 if m % 8 == 0
      use_synth :prophet
      play chord(:e3, :minor).choose, attack: 0, release: 0.08, cutoff: rrand_i(110, 130)
      sleep 0.125
    end
  end

  cue :quux
  in_thread do
    use_random_seed 668
    with_fx :echo, phase: 0.125 do
      16.times do
        use_synth :tb303
        play chord(:e3, :minor).choose, attack: 0, release: 0.1, cutoff: rrand(50, 100)
        sleep 0.25
      end
    end
  end

  sleep 4
end
