
$:.unshift File.dirname(__FILE__) + "/../lib"
require 'parslet'

include Parslet

parser = str('a').capture(:a) >> scope { str('b').capture(:a) } >> 
  dynamic { |s,c| str(c.captures[:a]) }
  
begin
  parser.parse('aba')
  puts "parses 'aba'"
rescue
  puts "exception!"
end