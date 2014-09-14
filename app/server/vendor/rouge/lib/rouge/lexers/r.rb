# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    class R < RegexLexer
      desc 'The R statistics language (r-project.org)'
      tag 'r'
      aliases 'r', 'R', 's', 'S'
      filenames '*.R', '*.S', '.Rhistory', '.Rprofile'
      mimetypes 'text/x-r-source', 'text/x-r', 'text/x-R'

      mimetypes 'text/x-r', 'application/x-r'

      def self.keywords
        @keywords ||= %w(
          if else for while repeat in next break return switch function
        )
      end

      def self.analyze_text(text)
        return 1 if text.shebang? 'Rscript'
        return 0.1 if text.include? '->'
      end

      state :root do
        rule /#.*?\n/, Comment::Single
        rule /\s+/m, Text
        rule /[.]?[a-zA-Z_][\w.]*/ do |m|
          if self.class.keywords.include? m[0]
            token Keyword
          else
            token Name
          end
        end

        rule /`.*?`/, Str::Backtick
        rule /'(\\.|.)*?'/m, Str::Single
        rule /"(\\.|.)*?"/m, Str::Double

        rule /\b(NULL|Inf|TRUE|FALSE|NaN)\b/, Keyword::Constant
        rule /\bNA(_(integer|real|complex|character)_)?\b/,
          Keyword::Constant
        rule /\b[TF]\b/, Keyword::Variable

        rule /0[xX][a-fA-F0-9]+([pP][0-9]+)?[Li]?/, Num::Hex
        rule /[+-]?(\d+([.]\d+)?|[.]\d+)([eE][+-]?\d+)?[Li]?/,
          Num

        rule /[\[\]{}();,]/, Punctuation

        rule %r([-<>?*+^/!=~$@:%&|]), Operator
        rule /[.][.][.]/, Keyword
      end
    end
  end
end
