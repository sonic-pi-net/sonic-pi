
# Paints a graphviz graph of your parser.

begin
  require 'ruby-graphviz'
rescue LoadError
  puts "Please install the 'ruby-graphviz' gem first."
  fail
end

require 'set'
require 'parslet/atoms/visitor'

module Parslet
  class GraphvizVisitor
    def initialize g
      @graph = g
      @known_links = Set.new
      @visited = Set.new
    end

    attr_reader :parent

    def visit_parser(root)
      recurse root, node('parser')
    end
    def visit_entity(name, block)
      s = node(name)

      downwards s

      return if @visited.include?(name)
      @visited << name

      recurse block.call, s
    end
    def visit_named(name, atom)
      recurse atom, parent
    end
    def visit_repetition(tag, min, max, atom)
      recurse atom, parent
    end
    def visit_alternative(alternatives)
      p = parent
      alternatives.each do |atom|
        recurse atom, p
      end
    end
    def visit_sequence(sequence)
      p = parent
      sequence.each do |atom|
        recurse atom, p
      end
    end
    def visit_lookahead(positive, atom)
      recurse atom, parent
    end
    def visit_re(regexp)
      # downwards node(regexp.object_id, label: escape("re(#{regexp.inspect})"))
    end
    def visit_str(str)
      # downwards node(str.object_id, label: escape("#{str.inspect}"))
    end

    def escape str
      str.gsub('"', "'")
    end
    def node name, opts={}
      @graph.add_nodes name.to_s, opts
    end
    def downwards child
      if @parent && !@known_links.include?([@parent, child])
        @graph.add_edges(@parent, child)
        @known_links << [@parent, child]
      end
    end
    def recurse node, current
      @parent = current
      node.accept(self)
    end
  end

  module Graphable
    def graph opts
      g = GraphViz.new(:G, type: :digraph)
      visitor = GraphvizVisitor.new(g)

      new.accept(visitor)

      g.output opts
    end
  end

  class Parser # reopen for introducing the .graph method
    extend Graphable
  end
end