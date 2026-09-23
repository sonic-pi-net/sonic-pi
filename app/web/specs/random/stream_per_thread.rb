# each thread has its own position; the parent's is unaffected by the child's draws
use_random_seed 3
in_thread do
  5.times { rand }
  puts rand
end
sleep 0.1
puts rand
