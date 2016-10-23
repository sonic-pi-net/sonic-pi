# rand-seed-ver 33
#
# Coded by Sam Aaron
#
# Video: https://vimeo.com/110416910

use_debug false
load_samples [:bd_haus, :elec_blip, :ambi_lunar_land]

define :ocean do |num, amp_mul=1|
  num.times do
    s = synth [:bnoise, :cnoise, :gnoise].choose, amp: rrand(0.5, 1.5) * amp_mul, attack: rrand(0, 1), sustain: rrand(0, 2), release: rrand(0, 5) + 0.5, cutoff_slide: rrand(0, 5), cutoff: rrand(60, 100), pan: rrand(-1, 1), pan_slide: 1
    control s, pan: rrand(-1, 1), cutoff: rrand(60, 110)
    sleep rrand(0.5, 4)
  end
end

define :echoes do |num, tonics, co=100, res=0.9, amp=1|
  num.times do
    play chord(tonics.choose, :minor).choose, res: res, cutoff: rrand(co - 20, co + 20), amp: 0.5 * amp, attack: 0, release: rrand(0.5, 1.5), pan: rrand(-0.7, 0.7)
    sleep [0.25, 0.5, 0.5, 0.5, 1, 1].choose
  end
end

define :bd do
  cue :in_relentless_cycles
  16.times do
    sample :bd_haus, amp: 4, cutoff: 100
    sleep 0.5
  end
  cue :winding_everywhichway
  2.times do
    2.times do
      sample :bd_haus, amp: 4, cutoff: 100
      sleep 0.25
    end
    sample :ambi_lunar_land
    sleep 0.25
  end
end

define :drums do |level, b_level=1, rand_cf=false|
  synth :fm, note: :e2, release: 0.1, amp: b_level * 3, cutoff: 130
  co = rand_cf ? rrand(110, 130) : 130
  a  = rand_cf ? rrand(0.3, 0.5) : 0.6
  n  = rand_cf ? :bnoise         : :noise
  synth :noise, release: 0.05, cutoff: co, res: 0.95, amp: a if level > 0
  sample :elec_blip, amp: 2, rate: 2, pan: rrand(-0.8, 0.8) if level > 1
  sleep 1
end

define :synths do |s_name, co, n=:e2|
  use_synth s_name
  use_transpose 0
  use_synth_defaults detune: [12,24].choose, amp: 1, cutoff: co, pulse_width: 0.12, attack: rrand(0.2, 0.5), release: 0.5 ,  mod_phase: 0.25, mod_invert_wave: 1

  play :e1, mod_range: [7, 12].choose, pan: rrand(-1, 1)
  sleep 0.125

  play :e3, mod_range: [7, 12].choose, pan: rrand(-1, 1)
  sleep [0.25, 0.5].choose

  play n, mod_range: 12, pan: rrand(-1, 1)
  sleep 0.5

  play chord(:e2, :minor).choose, mod_range: 12, pan: rrand(-1, 1)
  sleep 0.25
end

define :play_synths do
  with_fx :reverb do |r|
    with_fx :echo, phase: 0.25 do |e|
      synths = [:mod_pulse, :mod_saw, :mod_dsaw, :mod_dsaw, :mod_dsaw, :mod_dsaw]
      cutoffs = [108, 78, 88, 98]
      synth = synths.rotate!.first
      4.times do |t|
        puts shuffle("0" * (30 - t) + ("1" * t)) unless t == 0
        co = cutoffs.rotate!.first + (t * 2)
        7.times do
          n = chord([:e2, :e3, :e4, :e5][t], :minor).choose
          synths(synth, co, n)
        end
        sleep 2
      end
      sleep 1
      cue :within
    end
  end
end

define :binary_celebration do |n=1, st=1|
  in_thread do
    n.times do
      puts (0..30).map{|_| ["0", "1"].choose}.join
      sleep st
    end
  end
end

puts 'Introduction'
puts 'The Curved Ebb of Carpentry'
sleep 2

cue :oceans
at [7, 12], [:crash, :within_oceans] do |m|
  cue m
end

uncomment do
  use_random_seed 1000
  with_bpm 45 do
    with_fx :reverb do
      with_fx(:echo, delay: 0.5, decay: 4) do
        in_thread do
          use_random_seed 2
          ocean 5
          ocean 1, 0.5
          ocean 1, 0.25
        end
        sleep 10
        use_random_seed 1200
        echoes(5, [:b1, :b2, :e1, :e2, :b3, :e3])
        cue :a_distant_object
        echoes(5, [:b1, :e1, :e2, :e3])
        cue :breathes_time
        in_thread do
          echoes(5, [:e1, :e2, :e3])
        end
        use_synth :tb303
        echoes(1, [:e1, :e2, :e3], 60, 0.9, 0.5)
        echoes(1, [:e1, :e2, :e3], 62)
        echoes(1, [:e1, :e2, :e3], 64, 0.97)
        echoes(1, [:e1, :e2, :e3], 66)
        echoes(1, [:e1, :e2, :e3], 68)
        cue :liminality_holds_fast
        echoes(4, [:b1, :e1, :e2, :b3, :e3], 80)
        echoes(1, [:b1, :b2, :e1, :e2, :b3, :e3], 85,  0.98)
        cue :within_reach
        echoes(5, [:e1, :b2], 90)
        cue :as_it_unfolds
        in_thread do
          echoes(5, [:e1], 90)
        end
      end
    end
  end
end


in_thread(name: :bassdrums) do
  use_random_seed 0
  sleep 22
  3.times do
    bd
  end
  sleep 28
  live_loop :bd do
    bd
  end
end

in_thread(name: :drums) do
  use_random_seed 0
  level = -1
  with_fx :echo do |e|
    sleep 2
    drums -1, 0.1
    drums -1, 0.2
    drums -1, 0.4
    drums -1, 0.7
    puts "Part 2"
    puts "Inside the Machine"
    3.times do
      8.times do
        drums level, 0.8
      end
      6.times do
        drums(level)
      end

      sleep 1
      level += 1
    end
    sleep 4
    cue :dreams
    8.times do
      drums 1, 1, true
    end

    10.times do
      m = choose [shuffle(:within_dreams), :within_dreams, :dreams_within]
      cue m
      drums 2, 1, true
    end

    6.times do
      m = choose [shuffle("within") + "_dreams", :within_dreams.shuffle, "dreams_" + shuffle("within")]
      cue m
      drums 2
    end

    live_loop :drums do
      8.times do |i|
        drums 1
      end

      16.times do |i|
        cue " " * rand_i(32)
        at 1 do
          cue "  " * i
        end
        drums 2
      end
    end
  end
end

in_thread name: :synths do
  use_random_seed 0
  sleep 12
  cue :the_flow_of_logic
  play_synths
end

in_thread do
  use_random_seed 0
  sync :within
  puts "Part 3"
  puts "Reality A"
  sleep 12
  use_synth_defaults phase: 0.5, res: 0.5, cutoff: 80, release: 3.3, wave: 1

  2.times do
    [80, 90, 100, 110].each do |cf|
      use_merged_synth_defaults cutoff: cf
      puts "1" * 30
      synth :zawa, note: :e2, phase: 0.25
      synth :zawa, note: :a1
      sleep 3
    end
    4.times do |t|
      binary_celebration(6, 0.5)
      synth :zawa, note: :e2, phase: 0.25, res: rrand(0.8, 0.9), cutoff: [100, 105, 110, 115][t]
      sleep 3
    end
  end

  puts 'Part n'
  puts 'The Observer becomes the Observed'
  # Your turn...
end
