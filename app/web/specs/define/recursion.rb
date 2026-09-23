# a function may call itself
define :countdown do |n|
  return if n == 0
  play 60 + n
  sleep 0.1
  countdown n - 1
end
countdown 3
