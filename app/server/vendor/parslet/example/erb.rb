# Example that demonstrates how a simple erb-like parser could be constructed. 

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'parslet'

class ErbParser < Parslet::Parser
  rule(:ruby) { (str('%>').absent? >> any).repeat.as(:ruby) }
  
  rule(:expression) { (str('=') >> ruby).as(:expression) }
  rule(:comment) { (str('#') >> ruby).as(:comment) }
  rule(:code) { ruby.as(:code) }
  rule(:erb) { expression | comment | code }
  
  rule(:erb_with_tags) { str('<%') >> erb >> str('%>') }
  rule(:text) { (str('<%').absent? >> any).repeat(1) }
  
  rule(:text_with_ruby) { (text.as(:text) | erb_with_tags).repeat.as(:text) }
  root(:text_with_ruby)
end

parser = ErbParser.new
p parser.parse "The value of x is <%= x %>."
p parser.parse "<% 1 + 2 %>"
p parser.parse "<%# commented %>"
 

evaluator = Parslet::Transform.new do
  
  erb_binding = binding
  
  rule(:code => { :ruby => simple(:ruby) }) { eval(ruby, erb_binding); '' }  
  rule(:expression => { :ruby => simple(:ruby) }) { eval(ruby, erb_binding) }
  rule(:comment => { :ruby => simple(:ruby) }) { '' }
  
  rule(:text => simple(:text)) { text }
  rule(:text => sequence(:texts)) { texts.join }
  
end

puts evaluator.apply(parser.parse(<<-ERB
The <% a = 2 %>not printed result of "a = 2".
The <%# a = 1 %>not printed non-evaluated comment "a = 1", see the value of a below.
The <%= 'nicely' %> printed result.
The <% b = 3 %>value of a is <%= a %>, and b is <%= b %>.
ERB
))
