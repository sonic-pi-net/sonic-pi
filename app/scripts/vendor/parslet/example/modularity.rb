$:.unshift File.dirname(__FILE__) + "/../lib"

require 'pp'
require "parslet"

# Demonstrates modular parsers, split out over many classes. Please look at
# ip_address.rb as well.

module ALanguage
  include Parslet
    
  # Parslet rules are really a special kind of method. Mix them into your
  # classes!
  rule(:a_language) { str('aaa') }
end

# Parslet parsers are parslet atoms as well. Create an instance and chain them
# to your other rules. 
#
class BLanguage < Parslet::Parser
  root :blang
  
  rule(:blang) { str('bbb') }
end

# Parslet atoms are really Ruby values, pass them around. 
c_language = Parslet.str('ccc')
  
class Language < Parslet::Parser
  def initialize(c_language)
    @c_language = c_language
    super()
  end
  
  root :root

  include ALanguage
  
  rule(:root) { str('a(') >> a_language >> str(')') >> space |
                str('b(') >> BLanguage.new >> str(')') >> space | 
                str('c(') >> @c_language >> str(')') >> space }
  rule(:space) { str(' ').maybe }
end

Language.new(c_language).parse('a(aaa)')
Language.new(c_language).parse('b(bbb)')
Language.new(c_language).parse('c(ccc)')