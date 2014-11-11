require 'interception'
listener = lambda{ |exception, binding|
  puts "raised: #{exception.inspect}"
}

Interception.listen(listener)

begin
  raise "oopsy"
rescue => exception
  puts "rescued: #{exception.inspect}"
end

raise "daisy"

Interception.unlisten(listener)
