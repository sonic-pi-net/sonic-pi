# A small example on how to parse common types of comments. The example
# started out with parser code from Stephen Waits. 

$:.unshift File.dirname(__FILE__) + "/../lib"

require 'pp'
require 'parslet'
require 'parslet/convenience'

class ALanguage < Parslet::Parser
  root(:lines)
  
  rule(:lines) { line.repeat }
  rule(:line) { spaces >> expression.repeat >> newline }
  rule(:newline) { str("\n") >> str("\r").maybe }
  
  rule(:expression) { (str('a').as(:a) >> spaces).as(:exp) }
  
  rule(:spaces) { space.repeat }
  rule(:space) { multiline_comment | line_comment | str(' ') }
  
  rule(:line_comment) { (str('//') >> (newline.absent? >> any).repeat).as(:line) }
  rule(:multiline_comment) { (str('/*') >> (str('*/').absent? >> any).repeat >> str('*/')).as(:multi) }
end

code = %q(
  a
  // line comment
  a a a // line comment
  a /* inline comment */ a 
  /* multiline
  comment */
)

pp ALanguage.new.parse_with_debug(code)
