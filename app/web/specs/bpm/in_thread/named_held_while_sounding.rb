# a named thread keeps its name until its own sounds have ended, not just its code
use_bpm 120   # at twice the tempo: specs/in_thread/named_held_while_sounding.rb
in_thread(name: :short) do
  play 60
end
sleep 0.5
in_thread(name: :short) do
  play 62
end
