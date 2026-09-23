# reverb's tail follows its room, gverb's its release, ping_pong's its feedback and phase
use_bpm 120   # at twice the tempo: specs/with_fx/kill_delays.rb
with_fx :reverb, room: 0.05 do
  play 60, release: 0.25
end
with_fx :gverb, release: 0.5 do
  play 62, release: 0.25
end
with_fx :ping_pong, feedback: 0.1, phase: 0.25 do
  play 64, release: 0.5
end
