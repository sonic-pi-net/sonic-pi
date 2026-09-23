# clear puts a thread's settings, random stream and ticks at their defaults, not what it inherited
use_bpm 120   # at twice the tempo: specs/lang/clear.rb
use_synth :blade
use_octave 1
use_bpm 240
tick
puts rand

in_thread do
  use_transpose 3
  tick
  clear
  puts current_synth
  puts current_octave
  puts current_transpose
  puts current_bpm
  puts current_random_source
  puts rand
  puts tick
  play 60
  sleep 0.5
  play 62
end

sleep 0.25
clear
puts current_synth
puts current_octave
puts current_bpm
puts rand
puts tick
play 60
in_thread do
  puts current_synth
  puts rand
end
