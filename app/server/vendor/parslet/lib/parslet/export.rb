# Allows exporting parslet grammars to other lingos. 

require 'set'
require 'parslet/atoms/visitor'

class Parslet::Parser
  module Visitors
    class Citrus
      attr_reader :context, :output
      def initialize(context)
        @context = context
      end
      
      def visit_str(str)
        "\"#{str.inspect[1..-2]}\""
      end
      def visit_re(match)
        match.to_s
      end

      def visit_entity(name, block)
        context.deferred(name, block)

        "(#{context.mangle_name(name)})"
      end
      def visit_named(name, parslet)
        parslet.accept(self)
      end

      def visit_sequence(parslets)
        '(' <<
        parslets.
          map { |el| el.accept(self) }.
          join(' ') <<
        ')'
      end
      def visit_repetition(tag, min, max, parslet)
        parslet.accept(self) << "#{min}*#{max}"
      end
      def visit_alternative(alternatives)
        '(' <<
        alternatives.
          map { |el| el.accept(self) }.
          join(' | ') <<
        ')'
      end

      def visit_lookahead(positive, bound_parslet)
        (positive ? '&' : '!') <<
        bound_parslet.accept(self)
      end
    end

    class Treetop < Citrus
      def visit_repetition(tag, min, max, parslet)
        parslet.accept(self) << "#{min}..#{max}"
      end

      def visit_alternative(alternatives)
        '(' <<
        alternatives.
          map { |el| el.accept(self) }.
          join(' / ') <<
        ')'
      end
    end
  end

  # A helper class that formats Citrus and Treetop grammars as a string. 
  #
  class PrettyPrinter
    attr_reader :visitor
    def initialize(visitor_klass)
      @visitor = visitor_klass.new(self)
    end

    # Pretty prints the given parslet using the visitor that has been
    # configured in initialize. Returns the string representation of the
    # Citrus or Treetop grammar.
    #
    def pretty_print(name, parslet)
      output = "grammar #{name}\n"
      
      output << rule('root', parslet)
      
      seen = Set.new
      loop do
        # @todo is constantly filled by the visitor (see #deferred). We 
        # keep going until it is empty.
        break if @todo.empty?
        name, block = @todo.shift

        # Track what rules we've already seen. This breaks loops.
        next if seen.include?(name)
        seen << name

        output << rule(name, block.call)
      end
      
      output << "end\n"
    end
    
    # Formats a rule in either dialect. 
    #
    def rule(name, parslet)
      "  rule #{mangle_name name}\n" << 
      "    " << parslet.accept(visitor) << "\n" <<
      "  end\n"
    end
    
    # Whenever the visitor encounters an rule in a parslet, it defers the
    # pretty printing of the rule by calling this method. 
    #
    def deferred(name, content)
      @todo ||= []
      @todo << [name, content]
    end

    # Mangles names so that Citrus and Treetop can live with it. This mostly
    # transforms some of the things that Ruby allows into other patterns. If
    # there is collision, we will not detect it for now. 
    #
    def mangle_name(str)
      str.to_s.sub(/\?$/, '_p')
    end
  end

  # Exports the current parser instance as a string in the Citrus dialect. 
  #
  # Example: 
  #
  #   require 'parslet/export'
  #   class MyParser < Parslet::Parser
  #     root(:expression)
  #     rule(:expression) { str('foo') }
  #   end
  #   
  #   MyParser.new.to_citrus # => a citrus grammar as a string
  #
  def to_citrus
    PrettyPrinter.new(Visitors::Citrus).
      pretty_print(self.class.name, root)
  end

  # Exports the current parser instance as a string in the Treetop dialect. 
  #
  # Example: 
  #
  #   require 'parslet/export'
  #   class MyParser < Parslet::Parser
  #     root(:expression)
  #     rule(:expression) { str('foo') }
  #   end
  #   
  #   MyParser.new.to_treetop # => a treetop grammar as a string
  #
  def to_treetop
    PrettyPrinter.new(Visitors::Treetop).
      pretty_print(self.class.name, root)
  end
end

