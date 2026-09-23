# once a named thread has finished, and its sound has too, its name can be reused
in_thread(name: :short) do
  play 60
end
sleep 2
in_thread(name: :short) do
  play 62
end
