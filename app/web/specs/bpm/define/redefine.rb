# defining a name again replaces the function for every later call
use_bpm 120   # at twice the tempo: specs/define/redefine.rb
define :foo do
  play 60
end
foo
define :foo do
  play 72
end
foo
