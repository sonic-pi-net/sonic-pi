# a thread continues the parent's random stream from the spawn point
rrand_i(0, 100)
in_thread do
  play rrand_i(60, 80)
end
play rrand_i(60, 80)
