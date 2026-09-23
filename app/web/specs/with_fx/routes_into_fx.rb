# a synth in a with_fx plays into it; the fx is freed once the synth has ended, plus kill_delay
with_fx :level do
  play 60, release: 0.5
end
play 62
