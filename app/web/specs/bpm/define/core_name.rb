# a name from Sonic Pi's own API cannot be taken
use_bpm 120   # at twice the tempo: specs/define/core_name.rb
define :play do
  puts "never"
end
