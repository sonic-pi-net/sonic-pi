# A small example on how to make parslet ignore parts of the parse tree. 

$:.unshift File.dirname(__FILE__) + "/../lib"
require 'parslet'

class IgnoreParslet < Parslet::Atoms::Base
  def initialize(parslet)
    @parslet = parslet
  end
  def to_s_inner(prec)
    @parslet.to_s(prec)
  end
  def try(source, context, consume_all)
    success, value = result = @parslet.try(source, context, consume_all)
    
    return succ(nil) if success
    return result
  end
  
end
module IgnoreDSL
  def ignore
    IgnoreParslet.new(self)
  end
end

class Parslet::Atoms::Base
  include IgnoreDSL
end

include Parslet
p (str('a') >> str('b').ignore >> str('c')).
  parse('abc')