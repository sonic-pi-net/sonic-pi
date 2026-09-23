# a thread continues the parent's random stream from the spawn point
use_bpm 120   # at twice the tempo: specs/in_thread/random_stream_inherited.rb
rrand_i(0, 100)
in_thread do
  play rrand_i(60, 80)
end
play rrand_i(60, 80)
