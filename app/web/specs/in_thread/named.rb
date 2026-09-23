# a named thread carries its name into the trace
in_thread(name: :melody) do
  play 60
  sleep 0.25
  play 62
end
in_thread name: :bass do
  play 36
end
