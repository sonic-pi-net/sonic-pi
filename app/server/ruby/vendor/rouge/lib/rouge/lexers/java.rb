# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    class Java < RegexLexer
      desc "The Java programming language (java.com)"

      tag 'java'
      filenames '*.java'
      mimetypes 'text/x-java'

      keywords = %w(
        assert break case catch continue default do else finally for
        if goto instanceof new return switch this throw try while
      )

      declarations = %w(
        abstract const enum extends final implements native private protected
        public static strictfp super synchronized throws transient volatile
      )

      types = %w(boolean byte char double float int long short void)

      id = /[a-zA-Z_][a-zA-Z0-9_]*/

      state :root do
        rule %r(^
          (\s*(?:[a-zA-Z_][a-zA-Z0-9_.\[\]]*\s+)+?) # return arguments
          ([a-zA-Z_][a-zA-Z0-9_]*)                  # method name
          (\s*)(\()                                 # signature start
        )mx do |m|
          # TODO: do this better, this shouldn't need a delegation
          delegate Java, m[1]
          token Name::Function, m[2]
          token Text, m[3]
          token Punctuation, m[4]
        end

        rule /\s+/, Text
        rule %r(//.*?$), Comment::Single
        rule %r(/\*.*?\*/)m, Comment::Multiline
        rule /@#{id}/, Name::Decorator
        rule /(?:#{keywords.join('|')})\b/, Keyword
        rule /(?:#{declarations.join('|')})\b/, Keyword::Declaration
        rule /(?:#{types.join('|')})\b/, Keyword::Type
        rule /package\b/, Keyword::Namespace
        rule /(?:true|false|null)\b/, Keyword::Constant
        rule /(?:class|interface)\b/, Keyword::Declaration, :class
        rule /import\b/, Keyword::Namespace, :import
        rule /"(\\\\|\\"|[^"])*"/, Str
        rule /'(?:\\.|[^\\]|\\u[0-9a-f]{4})'/, Str::Char
        rule /(\.)(#{id})/ do
          groups Operator, Name::Attribute
        end
        rule /#{id}:/, Name::Label
        rule /\$?#{id}/, Name
        rule /[~^*!%&\[\](){}<>\|+=:;,.\/?-]/, Operator
        rule /[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?/, Num::Float
        rule /0x[0-9a-f]+/, Num::Hex
        rule /[0-9]+L?/, Num::Integer
        # rule /\n/, Text
      end

      state :class do
        rule /\s+/m, Text
        rule id, Name::Class, :pop!
      end

      state :import do
        rule /\s+/m, Text
        rule /[a-z0-9_.]+\*?/i, Name::Namespace, :pop!
      end
    end
  end
end
