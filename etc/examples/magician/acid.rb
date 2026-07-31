# Coded by Sam Aaron

live_loop :drums_n_bass, delay: 2 do
  use_synth :fm
  28.times do
    sample :drum_bass_hard, amp: 0.8
    sleep 0.25
    play :e2, release: 0.2
    sample :elec_cymbal, rate: 12, amp: 0.6
    sleep 0.25
  end
  sleep 4
end


with_fx :reverb do |rev|
  live_loop :walk  do
    use_synth :tb303
    control rev, mix: rrand(0, 0.3)
    with_fx :slicer, phase: 0.125 do
      sample :ambi_lunar_land, sustain: 0, release: 8, amp: 2
    end

    control rev, mix: rrand(0, 0.6)
    r = rrand(0.05, 0.3)
    64.times do
      play chord(:e3, :minor).choose, release: r, cutoff: rrand(50, 90), amp: 0.5
      sleep 0.125
    end

    control rev, mix: rrand(0, 0.6)
    r = rrand(0.1, 0.2)
    with_synth :prophet do
      32.times do
        sleep 0.125
        play chord(:a3, :m7).choose, release: r, cutoff: rrand(40, 130), amp: 0.7
      end
    end

    control rev, mix: rrand(0, 0.6)
    r = rrand(0.05, 0.3)
    32.times do
      play chord(:e3, :minor).choose, release: r, cutoff: rrand(110, 130), amp: 0.4
      sleep 0.125
    end

    control rev, mix: rrand(0, 0.6)
    with_fx :echo, phase: 0.25, decay: 8 do
      16.times do
        play chord([:e2, :e3, :e4].choose, :m7).choose, release: 0.05, cutoff: rrand(50, 129), amp: 0.5
        sleep 0.125
      end
    end
  end
end
