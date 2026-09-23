# reset puts a thread's settings, random stream and ticks back as they were when it started
puts current_sample_defaults
puts current_random_source
use_synth :blade
use_octave 1
use_transpose 2
use_sample_defaults amp: 0.5
use_random_source :pink
puts rand
puts tick
reset
puts current_synth
puts current_octave
puts current_transpose
puts current_sample_defaults
puts current_random_source
puts rand
puts tick
play 60

in_thread do
  use_synth :tb303
  use_octave 2
  use_cent_tuning 10
  puts rand
  puts rand
  tick
  tick
  use_bpm 120
  reset
  puts current_synth
  puts current_octave
  puts current_cent_tuning
  puts current_bpm
  puts rand
  puts look
  play 60
  sleep 0.5
  play 62
  density 2 do
    reset
    sleep 0.25
  end
  puts vt
end

sleep 0.25
use_synth :pluck
use_octave -1
in_thread do
  use_synth :saw
  reset
  puts current_synth
  puts current_octave
  in_thread do
    puts rand
  end
end
