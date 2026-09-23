# the first trigger of a sample that is not loaded yet happens in a helper
# thread spawned for the load; once loaded, triggers run in the calling thread
sample :loop_amen
sleep 0.25
sample :loop_amen
in_thread do
  sample :drum_heavy_kick
  sleep 0.25
  sample :drum_heavy_kick
end
