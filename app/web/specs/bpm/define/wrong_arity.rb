# too many or too few arguments
use_bpm 120   # at twice the tempo: specs/define/wrong_arity.rb
define :one do |a|
  a
end
puts one(1)
one(1, 2)
