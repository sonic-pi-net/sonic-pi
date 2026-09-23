# a string name and a symbol name are two names; each is held while its thread still sounds
use_bpm 120   # at twice the tempo: specs/in_thread/name_string_held.rb
in_thread(name: "drums") do
  play 36
end
sleep 0.5
in_thread(name: :drums) do
  play 99
end
sleep 0.5
in_thread(name: "drums") do
  play 50
end
