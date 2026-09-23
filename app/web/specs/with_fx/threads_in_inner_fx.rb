# a thread started inside nested blocks of one thread keeps both
with_fx :level, kill_delay: 0.25 do
  with_fx :echo, decay: 0.25 do
    in_thread do
      sleep 1
      play 60, release: 0.25
    end
  end
end
