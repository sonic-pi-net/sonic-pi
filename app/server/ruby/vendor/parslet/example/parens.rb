# A small example that demonstrates the power of tree pattern matching. Also
# uses '.as(:name)' to construct a tree that can reliably be matched
# afterwards. 

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'pp'
require 'parslet'

module LISP # as in 'lots of insipid and stupid parenthesis'
  class Parser < Parslet::Parser
    rule(:balanced) {
      str('(').as(:l) >> balanced.maybe.as(:m) >> str(')').as(:r)
    }
  
    root(:balanced)
  end

  class Transform < Parslet::Transform
    rule(:l => '(', :m => simple(:x), :r => ')') { 
      # innermost :m will contain nil
      x.nil? ? 1 : x+1
    }
  end
end

parser = LISP::Parser.new
transform = LISP::Transform.new
%w!
  ()
  (())
  ((((()))))
  ((())
!.each do |pexp|
  begin
    result = parser.parse(pexp)
    puts "#{"%20s"%pexp}: #{result.inspect} (#{transform.apply(result)} parens)"
  rescue Parslet::ParseFailed => m
    puts "#{"%20s"%pexp}: #{m}"
  end
  puts
end
