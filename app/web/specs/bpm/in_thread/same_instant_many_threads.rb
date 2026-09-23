# several threads reaching one instant together (their order is not defined)
use_bpm 120   # at twice the tempo: specs/in_thread/same_instant_many_threads.rb
3.times do |i|
  in_thread do
    sleep 0.5
    play 60 + i
  end
end
