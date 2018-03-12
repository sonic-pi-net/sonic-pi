# A simple parser generator library. Typical usage would look like this: 
#
#   require 'parslet'
#        
#   class MyParser < Parslet::Parser
#     rule(:a) { str('a').repeat }
#     root(:a)        
#   end
#        
#   pp MyParser.new.parse('aaaa')   # => 'aaaa'@0
#   pp MyParser.new.parse('bbbb')   # => Parslet::Atoms::ParseFailed: 
#                                   #    Don't know what to do with bbbb at line 1 char 1.
#
# The simple DSL allows you to define grammars in PEG-style. This kind of
# grammar construction does away with the ambiguities that usually comes with
# parsers; instead, it allows you to construct grammars that are easier to
# debug, since less magic is involved. 
#
# Parslet is typically used in stages: 
#
# 
# * Parsing the input string; this yields an intermediary tree, see
#   Parslet.any, Parslet.match, Parslet.str, Parslet::ClassMethods#rule and
#   Parslet::ClassMethods#root.
# * Transformation of the tree into something useful to you, see
#   Parslet::Transform, Parslet.simple, Parslet.sequence and Parslet.subtree.
#
# The first stage is traditionally intermingled with the second stage; output
# from the second stage is usually called the 'Abstract Syntax Tree' or AST. 
#
# The stages are completely decoupled; You can change your grammar around and
# use the second stage to isolate the rest of your code from the changes
# you've effected. 
#
# == Further reading
# 
# All parslet atoms are subclasses of {Parslet::Atoms::Base}. You might want to
# look at all of those: {Parslet::Atoms::Re}, {Parslet::Atoms::Str},
# {Parslet::Atoms::Repetition}, {Parslet::Atoms::Sequence},
# {Parslet::Atoms::Alternative}.
#
# == When things go wrong
#
# A parse that fails will raise {Parslet::ParseFailed}. This exception contains
# all the details of what went wrong, including a detailed error trace that 
# can be printed out as an ascii tree. ({Parslet::Cause})
#
module Parslet
  # Extends classes that include Parslet with the module
  # {Parslet::ClassMethods}.
  #
  def self.included(base)
    base.extend(ClassMethods)
  end
  
  # Raised when the parse failed to match. It contains the message that should
  # be presented to the user. More details can be extracted from the
  # exceptions #cause member: It contains an instance of {Parslet::Cause} that
  # stores all the details of your failed parse in a tree structure. 
  #
  #   begin
  #     parslet.parse(str)
  #   rescue Parslet::ParseFailed => failure
  #     puts failure.cause.ascii_tree
  #   end
  #
  # Alternatively, you can just require 'parslet/convenience' and call the
  # method #parse_with_debug instead of #parse. This method will never raise
  # and print error trees to stdout.
  #
  #   require 'parslet/convenience'
  #   parslet.parse_with_debug(str)
  #
  class ParseFailed < StandardError
    def initialize(message, cause=nil)
      super(message)
      @cause = cause
    end
    
    # Why the parse failed. 
    #
    # @return [Parslet::Cause]
    attr_reader :cause 
  end
  
  module ClassMethods
    # Define an entity for the parser. This generates a method of the same
    # name that can be used as part of other patterns. Those methods can be
    # freely mixed in your parser class with real ruby methods.
    # 
    #   class MyParser
    #     include Parslet
    #
    #     rule(:bar) { str('bar') }
    #     rule(:twobar) do
    #       bar >> bar
    #     end
    #
    #     root :twobar
    #   end
    #
    def rule(name, &definition)
      define_method(name) do
        @rules ||= {}     # <name, rule> memoization
        return @rules[name] if @rules.has_key?(name)
        
        # Capture the self of the parser class along with the definition.
        definition_closure = proc {
          self.instance_eval(&definition)
        }
        
        @rules[name] = Atoms::Entity.new(name, &definition_closure)
      end
    end
  end

  # Allows for delayed construction of #match. See also Parslet.match.
  #
  # @api private
  class DelayedMatchConstructor
    def [](str)
      Atoms::Re.new("[" + str + "]")
    end
  end
  
  # Returns an atom matching a character class. All regular expressions can be
  # used, as long as they match only a single character at a time. 
  #
  #   match('[ab]')     # will match either 'a' or 'b'
  #   match('[\n\s]')   # will match newlines and spaces
  #
  # There is also another (convenience) form of this method: 
  #
  #   match['a-z']      # synonymous to match('[a-z]')
  #   match['\n']       # synonymous to match('[\n]')
  #
  # @overload match(str)
  #   @param str [String] character class to match (regexp syntax)
  #   @return [Parslet::Atoms::Re] a parslet atom
  #
  def match(str=nil)
    return DelayedMatchConstructor.new unless str
    
    return Atoms::Re.new(str)
  end
  module_function :match
  
  # Returns an atom matching the +str+ given:
  #
  #   str('class')      # will match 'class' 
  #
  # @param str [String] string to match verbatim
  # @return [Parslet::Atoms::Str] a parslet atom
  # 
  def str(str)
    Atoms::Str.new(str)
  end
  module_function :str
  
  # Returns an atom matching any character. It acts like the '.' (dot)
  # character in regular expressions.
  #
  #   any.parse('a')    # => 'a'
  #
  # @return [Parslet::Atoms::Re] a parslet atom
  #
  def any
    Atoms::Re.new('.')
  end
  module_function :any
  
  # Introduces a new capture scope. This means that all old captures stay
  # accessible, but new values stored will only be available during the block
  # given and the old values will be restored after the block. 
  #
  # Example: 
  #   # :a will be available until the end of the block. Afterwards, 
  #   # :a from the outer scope will be available again, if such a thing 
  #   # exists. 
  #   scope { str('a').capture(:a) }
  #
  def scope(&block)
    Parslet::Atoms::Scope.new(block)
  end
  module_function :scope
  
  # Designates a piece of the parser as being dynamic. Dynamic parsers can
  # either return a parser at runtime, which will be applied on the input, or
  # return a result from a parse. 
  # 
  # Dynamic parse pieces are never cached and can introduce performance
  # abnormalitites - use sparingly where other constructs fail. 
  # 
  # Example: 
  #   # Parses either 'a' or 'b', depending on the weather
  #   dynamic { rand() < 0.5 ? str('a') : str('b') }
  #   
  def dynamic(&block)
    Parslet::Atoms::Dynamic.new(block)
  end
  module_function :dynamic

  # Returns a parslet atom that parses infix expressions. Operations are 
  # specified as a list of <atom, precedence, associativity> tuples, where 
  # atom is simply the parslet atom that matches an operator, precedence is 
  # a number and associativity is either :left or :right. 
  # 
  # Higher precedence indicates that the operation should bind tighter than
  # other operations with lower precedence. In common algebra, '+' has 
  # lower precedence than '*'. So you would have a precedence of 1 for '+' and
  # a precedence of 2 for '*'. Only the order relation between these two 
  # counts, so any number would work. 
  #
  # Associativity is what decides what interpretation to take for strings that
  # are ambiguous like '1 + 2 + 3'. If '+' is specified as left associative, 
  # the expression would be interpreted as '(1 + 2) + 3'. If right 
  # associativity is chosen, it would be interpreted as '1 + (2 + 3)'. Note 
  # that the hash trees output reflect that choice as well. 
  #
  # Example:
  #   infix_expression(integer, [add_op, 1, :left])
  #   # would parse things like '1 + 2'
  #
  # @param element [Parslet::Atoms::Base] elements that take the NUMBER position
  #    in the expression
  # @param operations [Array<(Parslet::Atoms::Base, Integer, {:left, :right})>]
  #  
  # @see Parslet::Atoms::Infix
  #
  def infix_expression(element, *operations)
    Parslet::Atoms::Infix.new(element, operations)
  end
  module_function :infix_expression
  
  # A special kind of atom that allows embedding whole treetop expressions
  # into parslet construction. 
  #
  #   # the same as str('a') >> str('b').maybe
  #   exp(%Q("a" "b"?))     
  #
  # @param str [String] a treetop expression
  # @return [Parslet::Atoms::Base] the corresponding parslet parser
  #
  def exp(str)
    Parslet::Expression.new(str).to_parslet
  end
  module_function :exp
  
  # Returns a placeholder for a tree transformation that will only match a
  # sequence of elements. The +symbol+ you specify will be the key for the
  # matched sequence in the returned dictionary.
  #
  #   # This would match a body element that contains several declarations.
  #   { :body => sequence(:declarations) }
  #
  # The above example would match <code>:body => ['a', 'b']</code>, but not
  # <code>:body => 'a'</code>. 
  #
  # see {Parslet::Transform}
  #
  def sequence(symbol)
    Pattern::SequenceBind.new(symbol)
  end
  module_function :sequence
  
  # Returns a placeholder for a tree transformation that will only match
  # simple elements. This matches everything that <code>#sequence</code>
  # doesn't match.
  #
  #   # Matches a single header. 
  #   { :header => simple(:header) }
  #
  # see {Parslet::Transform}
  #
  def simple(symbol)
    Pattern::SimpleBind.new(symbol)
  end
  module_function :simple
  
  # Returns a placeholder for tree transformation patterns that will match 
  # any kind of subtree. 
  #
  #   { :expression => subtree(:exp) }
  #
  def subtree(symbol)
    Pattern::SubtreeBind.new(symbol)
  end
  module_function :subtree

  autoload :Expression, 'parslet/expression'
end

require 'parslet/slice'
require 'parslet/cause'
require 'parslet/source'
require 'parslet/atoms'
require 'parslet/pattern'
require 'parslet/pattern/binding'
require 'parslet/transform'
require 'parslet/parser'
require 'parslet/error_reporter'
require 'parslet/scope'