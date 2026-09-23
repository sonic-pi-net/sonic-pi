# once a named thread has finished, and its sound has too, its name can be reused
use_bpm 120   # at twice the tempo: specs/in_thread/named_after_finish.rb
in_thread(name: :short) do
  play 60
end
sleep 2
in_thread(name: :short) do
  play 62
end
