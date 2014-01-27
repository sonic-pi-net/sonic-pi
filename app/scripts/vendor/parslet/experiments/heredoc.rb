# An example that demonstrates how you would parse Ruby heredocs.
# This is just a hack, however it toys around with some concepts that we 
# just might use later on. 

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'pp'
require 'parslet'
require 'parslet/convenience'

class Parslet::Atoms::Base
  def space
    self >> Parslet.str(' ').repeat
  end
  
  def bind(name)
    BoundParslet.new(self, name)
  end
  
  def matches(name)
    BindCompare.new(self, name)
  end
end

class BoundParslet < Parslet::Atoms::Base
  attr_reader :parslet, :name
  def initialize(parslet, name)
    super()
    @parslet, @name = parslet, name
  end
  
  def try(source, context)
    parslet.try(source, context).tap { |result| 
      set_binding(context, name, 
        flatten(result.result))
    }
  end
  
  def set_binding(context, name, value)
    b = context.instance_variable_get('@bindings') || {}
    b.store name, value
    p b
    context.instance_variable_set('@bindings', b)
  end
    
  def to_s_inner(prec)
    parslet.to_s(prec)
  end
end

class BindCompare < Parslet::Atoms::Base
  attr_reader :parslet, :name
  def initialize(parslet, name)
    super()
    @parslet, @name = parslet, name
  end
  
  def try(source, context)
    parslet.try(source, context).tap { |result| 
      unless result.error?
        value = flatten(result.result)
      
        p [value, bound_value(context, name), value == bound_value(context, name)]
        unless value == bound_value(context, name)
          p :error_return
          return error(source, "Bound value doesn't match.")
        end
      end
    }
  end
  
  def bound_value(context, name)
    b = context.instance_variable_get('@bindings') || {}
    b[name]
  end
    
  def to_s_inner(prec)
    parslet.to_s(prec)
  end
end

class HereDocs < Parslet::Parser
  root(:document)
  
  # a document of heredocs
  rule(:document) { heredoc.repeat }
  # a whole heredoc led by 'a' or 'b'
  rule(:heredoc)  { 
    space? >> 
      intro.as(:intro) >> 
      backticks >> 
      tag.bind(:tag) >> doc.as(:doc) | 
    space? >> eol
  }
  # essentially just 'a' or 'b'
  rule(:intro) { (str('a') | str('b')).space }
  # the tag that delimits the heredoc
  rule(:tag) { match['A-Z'].repeat(1) }
  # the doc itself, ends when tag is found at start of line
  rule(:doc) { gobble_eol >> doc_line }
  # a doc_line is either the stop tag followed by nothing 
  # or just any kind of line.
  rule(:doc_line) { 
    (end_tag.absent? >> gobble_eol).repeat >> end_tag
  }
  rule(:end_tag) { tag.matches(:tag) >> space? >> eol }
  # eats anything until an end of line is found
  rule(:gobble_eol) { (eol.absent? >> any).repeat >> eol }

  rule(:eol) { match['\n\r'].repeat(1) }
  rule(:space?) { str(' ').repeat }
  rule(:backticks) { str('<<').space }
end

code = %q(
  a <<HERE
  b <<THERE
HERE
  b <<THEN
  a <<HERE
THE
HERE
THEN
)

pp HereDocs.new.parse_with_debug(code)
