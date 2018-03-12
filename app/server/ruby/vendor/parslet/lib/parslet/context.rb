require 'blankslate'

# Provides a context for tree transformations to run in. The context allows
# accessing each of the bindings in the bindings hash as local method.
#
# Example: 
#
#   ctx = Context.new(:a => :b)
#   ctx.instance_eval do 
#     a # => :b
#   end
#
# @api private
class Parslet::Context < BlankSlate
  reveal :methods
  reveal :respond_to?
  reveal :inspect
  reveal :to_s
  reveal :instance_variable_set
  
  def meta_def(name, &body)
    metaclass = class <<self; self; end

    metaclass.send(:define_method, name, &body)
  end
  
  def initialize(bindings)
    bindings.each do |key, value|
      meta_def(key.to_sym) { value }
      instance_variable_set("@#{key}", value)
    end
  end
end