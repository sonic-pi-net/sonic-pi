require 'edn/string_transformer'
require 'edn/types'
require 'edn/metadata'
require 'bigdecimal'

module EDN
  class Transform < Parslet::Transform
    rule(:true => simple(:x)) { true }
    rule(:false => simple(:x)) { false }
    rule(:nil => simple(:x)) { nil }

    rule(:integer => simple(:num), :precision => simple(:n)) {
      Integer(num)
    }
    rule(:float => simple(:num), :precision => simple(:n)) {
      if n
        BigDecimal(num)
      else
        Float(num)
      end
    }

    rule(:string => simple(:x)) { EDN::StringTransformer.parse_string(x) }
    rule(:keyword => simple(:x)) { x.to_sym }
    rule(:symbol => simple(:x)) { EDN::Type::Symbol.new(x) }
    rule(:character => simple(:x)) {
      case x
      when "newline" then "\n"
      when "return" then "\r"
      when "tab" then "\t"
      when "space" then " "
      else x.to_s
      end
    }

    rule(:vector => subtree(:array)) { array }
    rule(:list => subtree(:array)) { EDN::Type::List.new(*array) }
    rule(:set => subtree(:array)) { Set.new(array) }
    rule(:map => subtree(:array)) { Hash[array.map { |hash| [hash[:key], hash[:value]] }] }

    rule(:tag => simple(:tag), :element => subtree(:element)) {
      EDN.tagged_element(tag.to_s, element)
    }

    rule(:metadata => subtree(:raw_metadata), :element => subtree(:element)) {
      metadata = raw_metadata.reverse.reduce({}) do |acc, m|
        case m
        when Symbol then acc.merge(m => true)
        when EDN::Type::Symbol then acc.merge(:tag => m)
        else acc.merge(m)
        end
      end
      element.extend EDN::Metadata
      element.metadata = metadata
      element
    }
  end
end
