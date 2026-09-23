# a named thread keeps its name until its own sounds have ended, not just its code
in_thread(name: :short) do
  play 60
end
sleep 0.5
in_thread(name: :short) do
  play 62
end
