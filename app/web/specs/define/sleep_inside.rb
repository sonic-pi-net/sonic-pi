# a function that sleeps advances the caller's time, in whichever thread calls it
define :rest_a_bit do
  sleep 0.5
end
play 60
rest_a_bit
play 62
in_thread do
  rest_a_bit
  play 64
end
