require 'interception'
def log_exceptions(&block)
  Interception.listen(block) do |exception, binding|
    puts "raised: #{exception.inspect} from #{binding.eval("__method__")}"
  end
end

def hello
  raise "oopsy"
rescue => exception
  puts "rescued: #{exception.inspect} in #{__method__}"
  raise "daisy"
end

log_exceptions do
  hello
end
