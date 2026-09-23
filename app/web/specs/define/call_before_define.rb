# calling a function before it exists is an error, with and without args
begin
  later
rescue NameError => e
  puts e.class.name
end
later(1)
