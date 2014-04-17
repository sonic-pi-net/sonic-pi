$:.unshift File.dirname(__FILE__) + "/../lib"

require "parslet"
require "pp"

# Parses strings like "var1 and (var2 or var3)" respecting operator precedence
# and parentheses. After that transforms the parse tree into an array of
# arrays like this:
#
# [["1", "2"], ["1", "3"]]
#
# The array represents a DNF (disjunctive normal form). Elements of outer
# array are connected with "or" operator, while elements of inner arrays are
# joined with "and".
#
class Parser < Parslet::Parser
  rule(:space)  { match[" "].repeat(1) }
  rule(:space?) { space.maybe }

  rule(:lparen) { str("(") >> space? }
  rule(:rparen) { str(")") >> space? }

  rule(:and_operator) { str("and") >> space? }
  rule(:or_operator)  { str("or")  >> space? }

  rule(:var) { str("var") >> match["0-9"].repeat(1).as(:var) >> space? }

  # The primary rule deals with parentheses.
  rule(:primary) { lparen >> or_operation >> rparen | var }

  # Note that following rules are both right-recursive.
  rule(:and_operation) { 
    (primary.as(:left) >> and_operator >> 
      and_operation.as(:right)).as(:and) | 
    primary }
    
  rule(:or_operation)  { 
    (and_operation.as(:left) >> or_operator >> 
      or_operation.as(:right)).as(:or) | 
    and_operation }

  # We start at the lowest precedence rule.
  root(:or_operation)
end

class Transformer < Parslet::Transform
  rule(:var => simple(:var)) { [[String(var)]] }

  rule(:or => { :left => subtree(:left), :right => subtree(:right) }) do
    (left + right)
  end

  rule(:and => { :left => subtree(:left), :right => subtree(:right) }) do
     res = []
     left.each do |l|
       right.each do |r|
         res << (l + r)
       end
     end
     res
  end
end

pp tree = Parser.new.parse("var1 and (var2 or var3)")
# {:and=>
#   {:left=>{:var=>"1"@3},
#    :right=>{:or=>{:left=>{:var=>"2"@13}, :right=>{:var=>"3"@21}}}}}
pp Transformer.new.apply(tree)
# [["1", "2"], ["1", "3"]]

