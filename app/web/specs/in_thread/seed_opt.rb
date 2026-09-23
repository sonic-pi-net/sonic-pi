# seed: sets the thread's random stream
in_thread(seed: 1) do
  play rrand_i(60, 80)
  play rrand_i(60, 80)
end
in_thread(seed: 1) do
  play rrand_i(60, 80)
end
