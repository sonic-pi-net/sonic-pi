# a thread started in the block ends at once, but is done only when its synth ends
with_fx :level, kill_delay: 0.25 do
  in_thread do
    play 60, release: 1
  end
end
