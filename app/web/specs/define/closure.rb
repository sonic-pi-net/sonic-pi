# a function closes over the variables around it
count = 0
define :tick_it do
  count += 1
  puts count
end
tick_it
tick_it
puts count
