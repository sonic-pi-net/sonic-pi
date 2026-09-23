# sleeping for a string is an error
use_bpm 120   # at twice the tempo: specs/sleep/bad_argument.rb
sleep "1"
play 60
