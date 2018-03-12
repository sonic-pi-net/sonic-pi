require 'parslet'
include Parslet

# parslet parses strings
str('foo').
  parse('foo') # => "foo"@0
  
# it matches character sets
match['abc'].parse('a') # => "a"@0
match['abc'].parse('b') # => "b"@0
match['abc'].parse('c') # => "c"@0

# and it annotates its output
str('foo').as(:important_bit).
  parse('foo') # => {:important_bit=>"foo"@0}
  
# you can construct parsers with just a few lines
quote = str('"')
simple_string = quote >> (quote.absent? >> any).repeat >> quote

simple_string.
  parse('"Simple Simple Simple"') # => "\"Simple Simple Simple\""@0
  
# or by making a fuss about it 
class Smalltalk < Parslet::Parser
  root :smalltalk
  
  rule(:smalltalk) { statements }
  rule(:statements) { 
    # insert smalltalk parser here (outside of the scope of this readme)
  }
end

# and then
Smalltalk.new.parse('smalltalk')

# be sure to read our 'Get started' tutorial at 
# http://kschiess.github.com/parslet/get-started.html