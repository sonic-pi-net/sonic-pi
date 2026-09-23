# every synth name resolves to its server synthdef name
use_bpm 120   # at twice the tempo: specs/play/every_synth.rb
[:bass_foundation, :bass_highend, :beep, :blade, :bnoise, :chipbass, :chiplead,
 :chipnoise, :cnoise, :dark_ambience, :dpulse, :dsaw, :dtri, :dull_bell, :fm,
 :gabberkick, :gnoise, :growl, :hollow, :hoover, :kalimba, :mod_beep, :mod_dsaw,
 :mod_fm, :mod_pulse, :mod_saw, :mod_sine, :mod_tri, :noise, :organ_tonewheel,
 :piano, :pluck, :pnoise, :pretty_bell, :prophet, :pulse, :rhodey, :rodeo, :saw,
 :sc808_bassdrum, :sc808_clap, :sc808_claves, :sc808_closed_hihat, :sc808_congahi,
 :sc808_congalo, :sc808_congamid, :sc808_cowbell, :sc808_cymbal, :sc808_maracas,
 :sc808_open_hihat, :sc808_rimshot, :sc808_snare, :sc808_tomhi, :sc808_tomlo,
 :sc808_tommid, :sine, :square, :subpulse, :supersaw, :tb303, :tech_saws, :tri,
 :winwood_lead, :zawa].each do |s|
  use_synth s
  play 60
end
