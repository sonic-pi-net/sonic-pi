# Example that demonstrates how a simple erb-like parser could be constructed. 

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'parslet'
require 'parslet/atoms/visitor'
require 'parslet/convenience'
require 'blankslate'

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

class Parslet::Source
  def match_excluding str
    slice_str = @str.check_until(Regexp.new(Regexp.escape(str)))
    return @str.rest_size unless slice_str
    return slice_str.size - str.size
  end
end

class AbsentParser < Parslet::Atoms::Base
  def initialize absent
    @absent = absent
  end

  def try(source, context, consume_all)
    excluding_length = source.match_excluding(@absent)

    if excluding_length >= 1
      return succ(source.consume(excluding_length))
    else
      return context.err(self, source, "Failed absence #{@absent.inspect}.")
    end
  end
end

class Parslet::Optimizer
  module DSL
    def >> other
      Match::Sequence.new(self, other)
    end
    def absent?
      Match::Lookahead.new(false, self)
    end
    def repeat(min=0, max=nil)
      Match::Repetition.new(self, min, max)
    end
  end
  module Match
    class Base
      include DSL

      def visit_parser(root)
        false
      end
      def visit_entity(name, block)
        false
      end
      def visit_named(name, atom)
        false
      end
      def visit_repetition(tag, min, max, atom)
        false
      end
      def visit_alternative(alternatives)
        false
      end
      def visit_sequence(sequence)
        false
      end
      def visit_lookahead(positive, atom)
        false
      end
      def visit_re(regexp)
        false
      end
      def visit_str(str)
        false
      end
      def match(other, bindings)
        @bindings = bindings
        other.accept(self)
      end
    end
    class Str < Base
      def initialize(variable)
        @variable = variable
      end
      def visit_str(str)
        if bound_value=@bindings[@variable]
          return bound_value == str
        else
          @bindings[@variable] = str
          return true
        end
      end
    end
    class Lookahead < Base
      def initialize(positive, expression)
        @positive, @expression = positive, expression
      end
      def visit_lookahead(positive, atom)
        positive == @positive && 
          @expression.match(atom, @bindings)
      end
    end
    class Sequence < Base
      def initialize(*parslets)
        @parslets = parslets
      end
      def visit_sequence(sequence)
        sequence.zip(@parslets).all? { |atom, expr| expr.match(atom, @bindings) }
      end
    end
    class Repetition < Base
      def initialize(expression, min, max)
        @min, @max, @expression = min, max, expression
      end
      def visit_repetition(tag, min, max, atom)
        @min == min && @max == max && @expression.match(atom, @bindings)
      end
    end
    class Re < Base
      def initialize(variable)
        @variable = variable
      end
      def visit_re(regexp)
        case @variable
          when Symbol
            p [@variable, regexp]
            fail
        else
          @variable == regexp
        end
      end
    end
  end

  def self.str(var)
    Match::Str.new(var)
  end
  def self.any
    Match::Re.new('.')
  end
  
  class Rule
    def initialize(expression, replacement)
      @expression, @replacement = expression, replacement
    end

    class Context < BlankSlate
      def initialize(bindings)
        @bindings = bindings
      end
      def method_missing(sym, *args, &block)
        if args.size == 0 && !block && @bindings.has_key?(sym)
          return @bindings[sym]
        end
        
        super
      end
      def call(callable)
        instance_eval(&callable)
      end
    end

    def match other
      bindings = {}
      if @expression.match(other, bindings)
        return bindings
      end
    end
    def call(bindings)
      context = Context.new(bindings)
      context.call(@replacement)
    end
  end
  def self.rule(expression, &replacement)
    rules << Rule.new(expression, replacement)
  end
  def self.rules
    @rules ||= []
  end
  def rules
    self.class.rules
  end

  class Transform
    def initialize(rules)
      @rules = rules
      @candidates = []
    end

    def default_parser(root)
      root.accept(self)
    end
    def default_entity(name, block)
      Parslet::Atoms::Entity.new(name) { block.call.accept(self) }
    end
    def default_named(name, atom)
      Parslet::Atoms::Named.new(atom.accept(self), name)
    end
    def default_repetition(tag, min, max, atom)
      Parslet::Atoms::Repetition.new(atom.accept(self), min, max, tag)
    end
    def default_alternative(alternatives)
      Parslet::Atoms::Alternative.new(
        *alternatives.map { |atom| atom.accept(self) })
    end
    def default_sequence(sequence)
      Parslet::Atoms::Sequence.new(
        *sequence.map { |atom| atom.accept(self) })
    end
    def default_lookahead(positive, atom)
      Parslet::Atoms::Lookahead.new(atom, positive)
    end
    def default_re(regexp)
      Parslet::Atoms::Re.new(regexp)
    end
    def default_str(str)
      Parslet::Atoms::Str.new(str)
    end

    def method_missing(sym, *args, &block)
      if (md=sym.to_s.match(/visit_([a-z]+)/)) && !block
        # Obtain the default, which is a completely transformed new parser
        default = self.send("default_#{md[1]}", *args)
        # Try transforming this parser again at the current level
        return transform(default)
      end

      super
    end
    def transform(atom)
      # Try to match one of the rules against the newly constructed tree.
      @rules.each do |rule|
        if bindings=rule.match(atom)
          return rule.call(bindings)
        end
      end

      # No match, returning new atom.
      return atom
    end
  end

  def apply(parser)
    parser.accept(Transform.new(rules))
  end
end
class Optimizer < Parslet::Optimizer
  rule((str(:x).absent? >> any).repeat(1)) {
    AbsentParser.new(x) }
end

parser = ErbParser.new
optimized_parser = Optimizer.new.apply(parser)
# p optimized_parser.parse(File.read(ARGV.first))
p parser.parse_with_debug(File.read(ARGV.first))
