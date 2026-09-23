# override: true evaluates a defonce again
use_bpm 120   # at twice the tempo: specs/define/defonce_override.rb
defonce :x do
  1
end
sleep 0.1
puts x
defonce :x, override: true do
  2
end
sleep 0.1
puts x
