$:.unshift File.dirname(__FILE__) + "/../lib"

require 'parslet'
require 'parslet/convenience'

# This example demonstrates tree error reporting in a real life example. 
# The parser code has been contributed by John Mettraux.

def prettify(str)
  puts " "*3 + " "*4 + "." + " "*4 + "10" + " "*3 + "." + " "*4 + "20"
  str.lines.each_with_index do |line, index|
    printf "%02d %s\n", 
      index+1, 
      line.chomp
  end
end

class Parser < Parslet::Parser

  # commons

  rule(:space) { match('[ \t]').repeat(1) }
  rule(:space?) { space.maybe }

  rule(:newline) { match('[\r\n]') }

  rule(:comment) { str('#') >> match('[^\r\n]').repeat }

  rule(:line_separator) {
    (space? >> ((comment.maybe >> newline) | str(';')) >> space?).repeat(1)
  }

  rule(:blank) { line_separator | space }
  rule(:blank?) { blank.maybe }

  rule(:identifier) { match('[a-zA-Z0-9_]').repeat(1) }

  # res_statement

  rule(:reference) {
    (str('@').repeat(1,2) >> identifier).as(:reference)
  }

  rule(:res_action_or_link) {
    str('.').as(:dot) >> (identifier >> str('?').maybe ).as(:name) >> str('()')
  }

  rule(:res_actions) {
    (
      reference
    ).as(:resources) >>
    (
      res_action_or_link.as(:res_action)
    ).repeat(0).as(:res_actions)
  }

  rule(:res_statement) {
    res_actions >>
    (str(':') >> identifier.as(:name)).maybe.as(:res_field)
  }

  # expression

  rule(:expression) {
    res_statement
  }

  # body

  rule(:body) {
    (line_separator >> (block | expression)).repeat(1).as(:body) >>
    line_separator
  }

  # blocks

  rule(:begin_block) {
    (str('concurrent').as(:type) >> space).maybe.as(:pre) >>
    str('begin').as(:begin) >>
    body >>
    str('end')
  }

  rule(:define_block) {
    str('define').as(:define) >> space >>
    identifier.as(:name) >> str('()') >>
    body >>
    str('end')
  }

  rule(:block) {
    define_block | begin_block
  }

  # root

  rule(:radix) {
    line_separator.maybe >> block >> line_separator.maybe
  }

  root(:radix)
end


ds = [
  %{
    define f()
      @res.name
    end
  },
  %{
    define f()
      begin
        @res.name
      end
    end
  }
]

ds.each do |d|

  puts '-' * 80
  prettify(d)

  parser = Parser.new

  begin
    parser.parse_with_debug(d)
  end
end

puts '-' * 80