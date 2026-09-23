# twenty threads, each playing once at its own offset
use_bpm 120   # at twice the tempo: specs/in_thread/many.rb
20.times do |i|
  in_thread do
    sleep i * 0.05
    play 60 + i
  end
end
