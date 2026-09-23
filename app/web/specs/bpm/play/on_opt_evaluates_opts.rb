# opts are still evaluated when on: is false (a side effect shows it)
use_bpm 120   # at twice the tempo: specs/play/on_opt_evaluates_opts.rb
x = 0
play 60, on: false, amp: (x += 1)
puts x
