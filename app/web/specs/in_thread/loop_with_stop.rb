# stop ends the thread it is called from
in_thread do
  loop do
    play 60
    sleep 0.25
    stop if current_beat >= 0.5
  end
end
sleep 0.25
play 72
