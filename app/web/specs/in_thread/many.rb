# twenty threads, each playing once at its own offset
20.times do |i|
  in_thread do
    sleep i * 0.05
    play 60 + i
  end
end
