# with arg checks on, an out-of-range opt is an error
use_bpm 120   # at twice the tempo: specs/play/use_arg_checks.rb
use_arg_checks true
play 60, amp: -1
