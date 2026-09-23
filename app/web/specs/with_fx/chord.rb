# every note of a chord plays into the fx and keeps it
with_fx :level, kill_delay: 0.25 do
  play chord(:e3, :minor), release: 0.5
end
