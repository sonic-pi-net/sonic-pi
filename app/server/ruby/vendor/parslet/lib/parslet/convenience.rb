class Parslet::Atoms::Base
  
  # Packages the common idiom
  #    
  #    begin
  #      tree = parser.parse('something')
  #    rescue Parslet::ParseFailed => error
  #      puts parser.cause.ascii_tree
  #    end
  #
  # into a convenient method.
  #
  # Usage:
  #   
  #   require 'parslet'
  #   require 'parslet/convenience'
  #   
  #   class FooParser < Parslet::Parser
  #     rule(:foo) { str('foo') }
  #     root(:foo)
  #   end
  #   
  #   FooParser.new.parse_with_debug('bar')
  #
  # @see Parslet::Atoms::Base#parse
  #
  def parse_with_debug str, opts={}
    parse str, opts
  rescue Parslet::ParseFailed => error
    puts error.cause.ascii_tree
  end

end