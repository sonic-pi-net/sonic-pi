# defonce evaluates its block once, in its own thread, and then returns that value
defonce :setup do
  puts "evaluating"
  42
end
puts setup
sleep 0.1
puts setup
defonce :setup do
  puts "again?"
  43
end
puts setup
