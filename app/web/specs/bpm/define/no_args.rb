# a function with no parameters, called with and without parentheses
use_bpm 120   # at twice the tempo: specs/define/no_args.rb
define :hi do
  puts "hi"
  play 60
end
hi
hi()
