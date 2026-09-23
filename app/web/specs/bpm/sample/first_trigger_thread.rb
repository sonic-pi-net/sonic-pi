# the first trigger of a sample that is not loaded yet happens in a helper
# thread spawned for the load; once loaded, triggers run in the calling thread
use_bpm 120   # at twice the tempo: specs/sample/first_trigger_thread.rb
sample :loop_amen
sleep 0.25
sample :loop_amen
in_thread do
  sample :drum_heavy_kick
  sleep 0.25
  sample :drum_heavy_kick
end
