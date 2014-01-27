require 'parslet'
require 'parslet/ignore'

module EDN
  class Parser < Parslet::Parser

    def parse_prefix(str, options={})
      source = Parslet::Source.new(str.to_s)
      success, value = setup_and_apply(source, nil)

      unless success
        reporter = options[:reporter] || Parslet::ErrorReporter::Tree.new
        success, value = setup_and_apply(source, reporter)

        fail "Assertion failed: success was true when parsing with reporter" if success
        value.raise
      end

      rest = nil
      if !source.eof?
        rest = source.consume(source.chars_left).to_s
      end

      return [flatten(value), rest]
    end

    root(:top)

    rule(:top) {
      space? >> element >> space?
    }

    rule(:element) {
      base_element |
      tagged_element |
      metadata.maybe >> metadata_capable_element.as(:element)
    }

    rule(:metadata_capable_element) {
      vector |
      list |
      set |
      map |
      symbol
    }

    rule(:tagged_element) {
      tag >> space? >> base_element.as(:element)
    }

    rule(:base_element) {
      vector |
      list |
      set |
      map |
      boolean |
      str('nil').as(:nil) |
      keyword |
      string |
      character |
      float |
      integer |
      symbol
    }

    # Collections

    rule(:vector) {
      str('[') >>
      top.repeat.as(:vector) >>
      space? >>
      str(']')
    }

    rule(:list) {
      str('(') >>
      top.repeat.as(:list) >>
      space? >>
      str(')')
    }

    rule(:set) {
      str('#{') >>
      top.repeat.as(:set) >>
      space? >>
      str('}')
    }

    rule(:map) {
      str('{') >>
      (top.as(:key) >> top.as(:value)).repeat.as(:map) >>
      space? >>
      str('}')
    }

    # Primitives

    rule(:integer) {
      (match['\-\+'].maybe >>
       (str('0') | match('[1-9]') >> digit.repeat)).as(:integer) >>
      (str('N') | str("M") ).maybe.as(:precision)
    }

    rule(:float) {
      (match['\-\+'].maybe >>
       (str('0') | (match('[1-9]') >> digit.repeat)) >>
       str('.') >> digit.repeat(1) >>
       (match('[eE]') >> match('[\-+]').maybe >> digit.repeat).maybe).as(:float) >>
      str('M').maybe.as(:precision)
    }

    rule(:string) {
      str('"') >>
      (str('\\') >> any | str('"').absent? >> any).repeat.as(:string) >>
      str('"')
    }

    rule(:character) {
      str("\\") >>
      (str('newline') | str('space') | str('tab') | str('return') |
       match['[:graph:]']).as(:character)
    }

    rule(:keyword) {
      str(':') >> symbol.as(:keyword)
    }

    rule(:symbol) {
      (symbol_chars >> (str('/') >> symbol_chars).maybe |
       str('/')).as(:symbol)
    }

    rule(:boolean) {
      str('true').as(:true) | str('false').as(:false)
    }

    # Parts

    rule(:metadata) {
      ((metadata_map | metadata_symbol | metadata_keyword) >> space?).repeat.as(:metadata)
    }

    rule(:metadata_map) {
      str('^{') >>
      ((keyword | symbol | string).as(:key) >> top.as(:value)).repeat.as(:map) >>
      space? >>
      str('}')
    }

    rule(:metadata_symbol) {
      str('^') >> symbol
    }

    rule(:metadata_keyword) {
      str('^') >> keyword
    }

    rule(:tag) {
      str('#') >> match['[:alpha:]'].present? >> symbol.as(:tag)
    }

    rule(:symbol_chars) {
      (symbol_first_char >>
       valid_chars.repeat) |
      match['\-\+\.']
    }

    rule(:symbol_first_char) {
      (match['\-\+\.'] >> match['0-9'].absent? |
       match['\#\:0-9'].absent?) >> valid_chars
    }

    rule(:valid_chars) {
      match['[:alnum:]'] | sym_punct
    }

    rule(:sym_punct) {
      match['\.\*\+\!\-\?\$_%&=:#']
    }

    rule(:digit) {
      match['0-9']
    }

    rule(:newline) { str("\r").maybe >> str("\n") }

    rule(:comment) {
      str(';') >> (newline.absent? >> any).repeat
    }

    rule(:discard) {
      str('#_') >> space? >> (tagged_element | base_element).ignore
    }

    rule(:space) {
      (discard | comment | match['\s,']).repeat(1)
    }

    rule(:space?) { space.maybe }
  end
end
