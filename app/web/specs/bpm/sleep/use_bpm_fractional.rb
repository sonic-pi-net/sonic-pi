# a beat need not be a clean decimal: 90 bpm makes it two thirds of a second,
# and a fractional tempo works too
use_bpm 120   # at twice the tempo: specs/sleep/use_bpm_fractional.rb
use_bpm 180
play 60
sleep 1
play 62
sleep 0.5
play 64
use_bpm 145.0
sleep 1
play 65
