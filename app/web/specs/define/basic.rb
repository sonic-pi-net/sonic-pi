# define makes a function; it takes arguments and returns its last value
define :twice do |n|
  n * 2
end
puts twice(21)
define :beep_at do |n, len|
  play n, release: len
  sleep len
end
beep_at 60, 0.25
beep_at 64, 0.5
