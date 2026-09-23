# a do block after a call on a parenthesised command belongs to the outer command, as in Ruby
with_fx :echo, mix: (line 0.1, 1, steps: 4).tick do
  play 60
end
with_fx :reverb, room: (ring 0.2, 0.8).mirror.tick do
  play 62
end
in_thread name: (ring :a, :b).tick do
  play 64
end
sleep 0.5
with_fx :echo, phase: (ring (quantise 0.3, 0.25), 0.5).tick do
  play 66
end
in_thread name: "#{(ring :c, :d).tick}_voice".to_sym do
  play 67
end
sleep 0.5
