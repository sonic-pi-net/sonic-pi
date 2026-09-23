# changing bpm after sleeping: beats continue, only the time scale changes
use_bpm 120   # at twice the tempo: specs/sleep/use_bpm_mid_run.rb
play 60
sleep 1
use_bpm 240
play 62
sleep 1
play 64
use_bpm 60
sleep 1
play 65
