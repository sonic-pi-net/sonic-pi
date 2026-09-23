# defining a name again replaces the function for every later call
define :foo do
  play 60
end
foo
define :foo do
  play 72
end
foo
