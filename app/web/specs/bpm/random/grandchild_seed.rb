# a thread's new threads are seeded counting on from its parent's count when it was spawned
use_bpm 120   # at twice the tempo: specs/random/grandchild_seed.rb
in_thread do
  puts rand
end
in_thread do
  puts rand
  in_thread do
    puts rand
  end
  in_thread do
    puts rand
  end
end
