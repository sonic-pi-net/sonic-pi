
# A demonstration of the new precedence climbing infix expression parser. 

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'pp'
require 'rspec'
require 'parslet'
require 'parslet/rig/rspec'
require 'parslet/convenience'

class InfixExpressionParser < Parslet::Parser
  root :variable_assignment_list

  rule(:space) { match[' '] }

  def cts atom
    atom >> space.repeat
  end
  def infix *args
    Infix.new(*args)
  end

  # This is the heart of the infix expression parser: real simple definitions
  # for all the pieces we need. 
  rule(:mul_op) { cts match['*/'] }
  rule(:add_op) { cts match['+-'] }
  rule(:digit) { match['0-9'] }
  rule(:integer) { cts digit.repeat(1).as(:int) }

  rule(:expression) { infix_expression(integer, 
    [mul_op, 2, :left], 
    [add_op, 1, :right]) }

  # And now adding variable assignments to that, just to a) demonstrate this
  # embedded in a bigger parser, and b) make the example interesting. 
  rule(:variable_assignment_list) { 
    variable_assignment.repeat(1) }
  rule(:variable_assignment) {
    identifier.as(:ident) >> equal_sign >> expression.as(:exp) >> eol }
  rule(:identifier) {
    cts (match['a-z'] >> match['a-zA-Z0-9'].repeat) }
  rule(:equal_sign) {
    cts str('=') }
  rule(:eol) {
    cts(str("\n")) | any.absent? }
end

class InfixInterpreter < Parslet::Transform
  rule(int: simple(:int)) { Integer(int) }
  rule(ident: simple(:ident), exp: simple(:result)) { |d| 
    d[:doc][d[:ident].to_s.strip.to_sym] = d[:result] }

  rule(l: simple(:l), o: /^\*/, r: simple(:r)) { l * r }
  rule(l: simple(:l), o: /^\+/, r: simple(:r)) { l + r }
end

input = <<ASSIGNMENTS
a = 1
b = 2
c = 3 * 25
d = 100 + 3*4
ASSIGNMENTS

puts input

int_tree = InfixExpressionParser.new.parse_with_debug(input)
bindings = {}
result   = InfixInterpreter.new.apply(int_tree, doc: bindings)

pp bindings
