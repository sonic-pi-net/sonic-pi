# several threads reaching one instant together (their order is not defined)
3.times do |i|
  in_thread do
    sleep 0.5
    play 60 + i
  end
end
