# The example from the readme. With this, I am making sure that the readme 
# 'works'. Is this too messy?

$:.unshift File.dirname(__FILE__) + "/../lib"

# cut here -------------------------------------------------------------------
require 'parslet'
include Parslet

# Constructs a parser using a Parser Expression Grammar like DSL: 
parser =  str('"') >> 
          (
            str('\\') >> any |
            str('"').absent? >> any
          ).repeat.as(:string) >> 
          str('"')
  
# Parse the string and capture parts of the interpretation (:string above)        
tree = parser.parse('"This is a \\"String\\" in which you can escape stuff"')

tree # => {:string=>"This is a \\\"String\\\" in which you can escape stuff"}

# Here's how you can grab results from that tree:

transform = Parslet::Transform.new do
  rule(:string => simple(:x)) { 
    puts "String contents: #{x}" }
end
transform.apply(tree)

