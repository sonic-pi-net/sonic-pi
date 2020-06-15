#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2019 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

# ixi lang was originally written by Thor Magnusson:
# https://github.com/thormagnusson/ixilang

# This is a port of some of the features adapted for
# embedding within Sonic Pi code.

# This port was created in collaboration with Thor.

require 'treetop'
require_relative "support/docsystem"


module SonicPiIxiLang
  class Block < Treetop::Runtime::SyntaxNode ; end
  class Expression < Treetop::Runtime::SyntaxNode ; end
  class Name < Treetop::Runtime::SyntaxNode ; end
  class PatternPerc < Treetop::Runtime::SyntaxNode ; end
  class LiveLoop < Treetop::Runtime::SyntaxNode ; end
end

Treetop.load_from_string("
grammar SonicPiIxiLang
  rule block
    expression+ <Block>
  end

  rule expression
    space? ll space? <Expression>
  end

  rule ll
    name '->' pat_perc <LiveLoop>
  end

  rule pat_perc
    [ \t]* '|' name+ '|' <PatternPerc>
  end

  rule name
    [ \t]* [a-zA-Z] [a-zA-Z0-9]* [ \t]* <Name>
  end

  rule space
    [\S\n]+
  end
end
")


module SonicPi
  module Lang
    module Ixi

      def self.included(base)
        base.instance_exec {alias_method :sonic_pi_mods_ixi_initialize_old, :initialize}

        base.instance_exec do
          define_method(:initialize) do |*splat, &block|
            sonic_pi_mods_ixi_initialize_old(*splat, &block)
            @ixi_methods = {}
          end
        end

      end

      def ixi_define(name, &block)
        @ixi_methods[name.to_sym] = block
      end

      def __parse_ixi(nodes, opts)
        case nodes
        when SonicPiIxiLang::Block
          nodes.elements.each {|n| __parse_ixi n, opts}
        when SonicPiIxiLang::Expression
          __parse_ixi(nodes.elements[1], opts)
        when SonicPiIxiLang::LiveLoop
          name = nodes.elements[0].text_value.strip
          live_loop "_ixi_#{name}" do
            # execute one lot of the pattern
            case nodes.elements[2]
            when SonicPiIxiLang::PatternPerc
              pat = nodes.elements[2].elements[2].text_value
              pat.size.times do |idx|
                sym = pat[idx].strip.to_sym
                behaviour = @ixi_methods[sym]
                if behaviour
                  case behaviour.arity
                    when 0
                    behaviour.call
                  else
                    behaviour.call(opts.clone)
                  end
                end

                sleep 0.125
              end

            else
              raise "Ixi Error. Unknown Ixi pattern form: #{nodes.elements[2].text_value}"
            end
          end
        else
          raise "Ixi Error. Unknown top level element: #{nodes.class}, #{nodes.inspect}"
        end


      end

      def ixi(s, opts={})

        parser = SonicPiIxiLangParser.new

        # assert parser.parse('hello->|a|')
        # assert parser.parse('yo -> |a b c a|')
        # assert parser.parse('yo ->|a b c a|')
        # assert parser.parse('yo->|a b c a|')
        # assert parser.parse('yo2 -> |a b c a|')
        # assert parser.parse('hello->|a|
        #                      hello2->|A|')

        # takes string s representing ixi lang commands
        # for example:
        # ixi "agent->|c c c c|"

        # ixi lang has:

        # name->|p a t t e r n|
        # which plays samples from a given directory.

        # Attempt to parse the ixi lang string:
        nodes = parser.parse(s)
        raise "Ixi Error. Unable to parse syntax: #{s}" unless nodes
        __parse_ixi(nodes, opts)
      end
    end
  end
end
