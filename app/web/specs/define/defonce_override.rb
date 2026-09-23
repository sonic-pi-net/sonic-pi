# override: true evaluates a defonce again
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
