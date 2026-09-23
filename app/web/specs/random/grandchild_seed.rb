# a thread's new threads are seeded counting on from its parent's count when it was spawned
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
