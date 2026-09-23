# samples in threads, like play
use_bpm 120   # at twice the tempo: specs/sample/in_thread.rb
in_thread do
  sleep 0.25
  sample :drum_heavy_kick
end
sample :loop_amen
