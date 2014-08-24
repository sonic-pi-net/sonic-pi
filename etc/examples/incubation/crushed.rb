with_fx :bitcrusher do
  loop do
    use_synth :mod_fm
    play 50 + [5, 0].choose, mod_phase: 0.25, release: 1, mod_range: [24, 27, 12].choose
    sleep 0.5
    use_synth :mod_dsaw
    play 50, mod_phase: 0.25, release: 1.5, mod_range: [24, 27].choose, attack: 0.5
    sleep 1.5
  end
end
