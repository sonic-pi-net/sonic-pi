# the Lisp-like forms of the fns that make rings read as calls, a string or comment naming one left alone
use_bpm 120   # at twice the tempo: specs/lang/preparser_lisp_rings.rb
play (ring 60, 64, 67).tick
play (scale :c4, :major).look
puts "(ring 1, 2)"
# (ring 3, 4)
play (knit 70, 2, 72, 1).reverse.first
