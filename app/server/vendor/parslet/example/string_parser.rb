# A more complex parser that illustrates how a compiler might be constructed.
# The parser recognizes strings and integer literals and constructs almost a
# useful AST from the file contents. 

require 'pp'

$:.unshift File.dirname(__FILE__) + "/../lib"
require 'parslet'

include Parslet

class LiteralsParser < Parslet::Parser
  rule :space do
    (match '[ ]').repeat(1)
  end
  
  rule :literals do
    (literal >> eol).repeat
  end
  
  rule :literal do
    (integer | string).as(:literal) >> space.maybe
  end
  
  rule :string do
    str('"') >> 
    (
      (str('\\') >> any) |
      (str('"').absent? >> any)
    ).repeat.as(:string) >> 
    str('"')
  end
  
  rule :integer do
    match('[0-9]').repeat(1).as(:integer)
  end
  
  rule :eol do
    line_end.repeat(1)
  end
  
  rule :line_end do
    crlf >> space.maybe
  end
  
  rule :crlf do
    match('[\r\n]').repeat(1)
  end

  root :literals
end

input_name = File.join(File.dirname(__FILE__), 'simple.lit')
file = File.read(input_name)

parsetree = LiteralsParser.new.parse(file)
  
class Lit < Struct.new(:text)
  def to_s
    text.inspect
  end
end
class StringLit < Lit
end
class IntLit < Lit
  def to_s
    text
  end
end

transform = Parslet::Transform.new do
  rule(:literal => {:integer => simple(:x)}) { IntLit.new(x) }
  rule(:literal => {:string => simple(:s)}) { StringLit.new(s) }
end
  
ast = transform.apply(parsetree)
pp ast
