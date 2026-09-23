# use_octave shifts by whole octaves; with_octave scopes it
use_bpm 120   # at twice the tempo: specs/play/use_octave.rb
play 60
use_octave 1
play 60
with_octave -2 do
  play 60
end
play 60
