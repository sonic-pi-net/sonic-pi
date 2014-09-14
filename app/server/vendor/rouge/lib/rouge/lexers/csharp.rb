# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    class CSharp < RegexLexer
      tag 'csharp'
      aliases 'c#', 'cs'
      filenames '*.cs'
      mimetypes 'text/x-csharp'

      desc 'a multi-paradigm language targeting .NET'

      # TODO: support more of unicode
      id = /@?[_a-z]\w*/i

      keywords = %w(
        abstract alias as base break case catch checked const continue
        default delegate do else enum event explicit extern false
        finally fixed for foreach global goto if implicit in interface
        internal is lock new null operator out override params private
        protected public readonly ref return sealed sizeof stackalloc
        static switch this throw true try typeof unchecked unsafe
        virtual void while get set new partial yield add remove value
      )

      keywords_type = %w(
        bool byte char decimal double float int long object sbyte
        short string uint ulong ushort
      )

      cpp_keywords = %w(
        if endif else elif define undef line error warning region
        endregion pragma
      )

      state :whitespace do
        rule /\s+/m, Text
        rule %r(//.*?\n), Comment::Single
        rule %r(/[*].*?[*]/)m, Comment::Multiline
      end

      state :root do
        mixin :whitespace

        rule /^\s*\[.*?\]/, Name::Attribute
        rule /[$]\s*"/, Str, :splice_string
        rule /[$]\s*<#/, Str, :splice_recstring
        rule /<#/, Str, :recstring

        rule /(<\[)\s*(#{id}:)?/, Keyword
        rule /\]>/, Keyword

        rule /[~!%^&*()+=|\[\]{}:;,.<>\/?-]/, Punctuation
        rule /@"(\\.|.)*?"/, Str
        rule /"(\\.|.)*?["\n]/, Str
        rule /'(\\.|.)'/, Str::Char
        rule /0x[0-9a-f]+[lu]?/i, Num
        rule %r(
          [0-9]
          ([.][0-9]*)? # decimal
          (e[+-][0-9]+)? # exponent
          [fldu]? # type
        )ix, Num
        rule /^#[ \t]*(#{cpp_keywords.join('|')})\b.*?\n/,
          Comment::Preproc
        rule /\b(#{keywords.join('|')})\b/, Keyword
        rule /\b(#{keywords_type.join('|')})\b/, Keyword::Type
        rule /class|struct/, Keyword, :class
        rule /namespace|using/, Keyword, :namespace
        rule /#{id}(?=\s*[(])/, Name::Function
        rule id, Name
      end

      state :class do
        mixin :whitespace
        rule id, Name::Class, :pop!
      end

      state :namespace do
        mixin :whitespace
        rule /(?=[(])/, Text, :pop!
        rule /(#{id}|[.])+/, Name::Namespace, :pop!
      end

    end
  end
end
