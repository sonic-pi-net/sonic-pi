# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    class LiterateHaskell < RegexLexer
      desc 'Literate haskell'
      tag 'literate_haskell'
      aliases 'lithaskell', 'lhaskell', 'lhs'
      filenames '*.lhs'
      mimetypes 'text/x-literate-haskell'

      def haskell
        @haskell ||= Haskell.new(options)
      end

      start { haskell.reset! }

      # TODO: support TeX versions as well.
      # TODO: enforce a blank line before and after code
      state :root do
        rule /\s*?\n(?=>)/, Text, :code
        rule /.*?\n/, Text
        rule /.+\z/, Text
      end

      state :code do
        rule /(>)( .*?\n)/ do |m|
          token Name::Label, m[1]
          delegate haskell, m[2]
        end

        rule /\s*\n(?=\s*[^>])/, Text, :pop!
      end
    end
  end
end
