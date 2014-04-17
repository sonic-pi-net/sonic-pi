
# @api private
module Parslet::Accelerator
  class Application
    def initialize atom, rules
      @atom = atom
      @rules = rules
    end

    def call
      @atom.accept(self)
    end

    def visit_parser(root)
      transform root.accept(self)
    end
    def visit_entity(name, block)
      transform Parslet::Atoms::Entity.new(name) { block.call.accept(self) }
    end
    def visit_named(name, atom)
      transform Parslet::Atoms::Named.new(atom.accept(self), name)
    end
    def visit_repetition(tag, min, max, atom)
      transform Parslet::Atoms::Repetition.new(atom.accept(self), min, max, tag)
    end
    def visit_alternative(alternatives)
      transform Parslet::Atoms::Alternative.new(
        *alternatives.map { |atom| atom.accept(self) })
    end
    def visit_sequence(sequence)
      transform Parslet::Atoms::Sequence.new(
        *sequence.map { |atom| atom.accept(self) })
    end
    def visit_lookahead(positive, atom)
      transform Parslet::Atoms::Lookahead.new(atom, positive)
    end
    def visit_re(regexp)
      transform Parslet::Atoms::Re.new(regexp)
    end
    def visit_str(str)
      transform Parslet::Atoms::Str.new(str)
    end

    def transform atom
      @rules.each do |expr, action|
        # Try and match each rule in turn
        binding = Parslet::Accelerator.match(atom, expr)
        if binding
          # On a successful match, allow the rule action to transform the
          # parslet into something new. 
          ctx = Parslet::Context.new(binding)
          return ctx.instance_eval(&action)
        end
      end # rules.each 

      # If no rule matches, this is the fallback - a clean new parslet atom.
      return atom
    end
  end
end

require 'parslet/context'