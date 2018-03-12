
# An exploration of two ideas: 
#   a) Constructing a whole parser inline, without the artificial class around
#      it. 
# and:
#   b) Constructing non-greedy or non-blind parsers by transforming the 
#      grammar.

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'parslet'
include Parslet

a =  str('a').repeat >> str('aa')

# E1% E2
# 
# S = E2 | E1 S 

def this(name, &block); return Parslet::Atoms::Entity.new(name, &block) end
def epsilon; any.absent? end 

# Traditional repetition will try as long as the pattern can be matched and 
# then give up. This is greedy and blind. 
a = str('a').as(:e) >> this('a') { a }.as(:rec) | epsilon

# Here's a pattern match that is greedy and non-blind. The first pattern
# 'a'* will be tried as many times as possible, while still matching the 
# end pattern 'aa'.
b = str('aa').as(:e2) >> epsilon | str('a').as(:e1) >> this('b') { b }.as(:rec)

p a.parse('aaaa')
p b
p b.parse('aaaa')
