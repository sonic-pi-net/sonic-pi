# use_cent_tuning shifts notes by hundredths of a semitone
use_bpm 120   # at twice the tempo: specs/play/cent_tuning.rb
use_cent_tuning 50
play 60
with_cent_tuning -25 do
  play 60
end
play 60
