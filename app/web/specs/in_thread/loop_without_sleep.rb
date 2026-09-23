# a loop that never sleeps or syncs is stopped with an error
in_thread do
  loop do
    play 60
  end
end
