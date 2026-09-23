# a synth in a with_fx plays into it; the fx is freed once the synth has ended, plus kill_delay
use_bpm 120   # at twice the tempo: specs/with_fx/routes_into_fx.rb
with_fx :level do
  play 60, release: 0.5
end
play 62
