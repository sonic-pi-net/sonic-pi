

# Optimizes the parsers by pattern matching on the parser atoms and replacing
# matches with better versions. See the file qed/accelerators.md for a more
# in-depth description.
#
# Example: 
#   quote = str('"')
#   parser = quote >> (quote.absent? >> any).repeat >> quote
#
#   A = Accelerator # for making what follows a bit shorter
#   optimized_parser = A.apply(parser, 
#     A.rule( (A.str(:x).absent? >> A.any).repeat ) { GobbleUp.new(x) })
#
#   optimized_parser.parse('"Parsing is now fully optimized! (tm)"')
#
module Parslet::Accelerator

  # An expression to match against a tree of parser atoms. Normally, an
  # expression is produced by Parslet::Accelerator.any, 
  # Parslet::Accelerator.str or Parslet::Accelerator.re.
  #
  # Expressions can be chained much like parslet atoms can be: 
  #
  #   expr.repeat(1)      # matching repetition
  #   expr.absent?        # matching absent?
  #   expr.present?       # matching present?
  #   expr1 >> expr2      # matching a sequence
  #   expr1 | expr2       # matching an alternation
  # 
  # @see Parslet::Accelerator.str
  # @see Parslet::Accelerator.re
  # @see Parslet::Accelerator.any
  #
  # @see Parslet::Accelerator
  # 
  class Expression
    attr_reader :type
    attr_reader :args

    def initialize(type, *args)
      @type = type
      @args = args
    end

    # @return [Expression]
    def >> other_expr
      join_or_new :seq, other_expr
    end

    # @return [Expression]
    def | other_expr
      join_or_new :alt, other_expr
    end

    # @return [Expression]
    def absent?
      Expression.new(:absent, self)
    end
    # @return [Expression]
    def present?
      Expression.new(:present, self)
    end

    # @return [Expression]
    def repeat min=0, max=nil
      Expression.new(:rep, min, max, self)
    end

    # @return [Expression]
    def as name
      Expression.new(:as, name)
    end

    # @api private
    # @return [Expression]
    def join_or_new tag, other_expr
      if type == tag
        @args << other_expr
      else
        Expression.new(tag, self, other_expr)
      end
    end
  end

module_function 
  # Returns a match expression that will match `str` parslet atoms.
  #
  # @return [Parslet::Accelerator::Expression]
  #
  def str variable, *constraints
    Expression.new(:str, variable, *constraints)
  end

  # Returns a match expression that will match `match` parslet atoms.
  #
  # @return [Parslet::Accelerator::Expression]
  #
  def re variable, *constraints
    Expression.new(:re, variable, *constraints)
  end

  # Returns a match expression that will match `any` parslet atoms.
  #
  # @return [Parslet::Accelerator::Expression]
  #
  def any
    Expression.new(:re, ".")
  end

  # Given a parslet atom and an expression, will determine if the expression
  # matches the atom. If successful, returns the bindings into the pattern
  # that were made. If no bindings had to be made to make the match successful, 
  # the empty hash is returned. 
  #
  # @param atom [Parslet::Atoms::Base] parslet atom to match against
  # @param expr [Parslet::Accelerator::Expression] expression to match
  # @return [nil, Hash] bindings for the match, nil on failure
  #
  def match atom, expr
    engine = Engine.new

    return engine.bindings if engine.match(atom, expr)
  end

  # Constructs an accelerator rule. A rule is a matching expression and the
  # code that should be executed once the expression could be bound to a 
  # parser. 
  #
  # Example: 
  #   Accelerator.rule(Accelerator.any) { Parslet.match('.') }
  #
  def rule expression, &action
    [expression, action]
  end

  # Given a parslet atom and a set of rules, tries to match the rules 
  # recursively through the parslet atom. Once a rule could be matched, 
  # its action block will be called.
  #
  # Example: 
  #   quote = str('"')
  #   parser = quote >> (quote.absent? >> any).repeat >> quote
  #
  #   A = Accelerator # for making what follows a bit shorter
  #   optimized_parser = A.apply(parser, 
  #     A.rule( (A.str(:x).absent? >> A.any).repeat ) { GobbleUp.new(x) })
  #
  #   optimized_parser.parse('"Parsing is now fully optimized! (tm)"')
  #
  # @param atom [Parslet::Atoms::Base] a parser to optimize
  # @param *rules [Parslet::Accelerator::Rule] rules produced by .rule
  # @return [Parslet::Atoms::Base] optimized parser
  #
  def apply atom, *rules
    Application.new(atom, rules).call
  end
end

require 'parslet/accelerator/engine'
require 'parslet/accelerator/application'