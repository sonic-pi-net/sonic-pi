# Please also look at the more naive 'erb.rb'. This shows how to optimize an
# ERB like parser using parslet. 

$:.unshift File.join(File.dirname(__FILE__), "/../lib")

require 'parslet'
require './qed/applique/gobbleup'
require 'parslet/accelerator'

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

A = Parslet::Accelerator
optimized = A.apply(parser, 
  A.rule((A.str(:x).absent? >> A.any).repeat(1)) { GobbleUp.new(x, 1) }, 
  A.rule((A.str(:x).absent? >> A.any).repeat(0)) { GobbleUp.new(x, 0) })

input = File.read(File.dirname(__FILE__) + "/big.erb")

# Remove the comment marks here to see what difference the optimisation makes.
# Commented out for the acceptance tests to run. 
#
# require 'benchmark'
# Benchmark.bm(7) do |bm|
#   bm.report('original') { parser.parse(input) }
#   bm.report('gobble') { optimized.parse(input) }
# end
p optimized.parse(input)