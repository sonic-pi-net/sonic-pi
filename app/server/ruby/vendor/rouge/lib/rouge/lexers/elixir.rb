# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    # Direct port of pygments Lexer.
    # See: https://bitbucket.org/birkenfeld/pygments-main/src/7304e4759ae65343d89a51359ca538912519cc31/pygments/lexers/functional.py?at=default#cl-2362
    class Elixir < RegexLexer
      desc "Elixir language (elixir-lang.org)"

      tag 'elixir'

      filenames '*.ex', '*.exs'

      mimetypes 'text/x-elixir', 'application/x-elixir'

      BRACES = [
        ['\{', '\}', 'cb'],
        ['\[', '\]', 'sb'],
        ['\(', '\)', 'pa'],
        ['\<', '\>', 'lt']
      ]

      state :root do
        rule /\s+/m, Text
        rule /#.*$/, Comment::Single
        rule %r{\b(case|cond|end|bc|lc|if|unless|try|loop|receive|fn|defmodule|
             defp?|defprotocol|defimpl|defrecord|defmacrop?|defdelegate|
             defexception|exit|raise|throw|unless|after|rescue|catch|else)\b(?![?!])|
             (?<!\.)\b(do|\-\>)\b}x, Keyword
        rule /\b(import|require|use|recur|quote|unquote|super|refer)\b(?![?!])/, Keyword::Namespace
        rule /(?<!\.)\b(and|not|or|when|xor|in)\b/, Operator::Word
        rule %r{%=|\*=|\*\*=|\+=|\-=|\^=|\|\|=|
             <=>|<(?!<|=)|>(?!<|=|>)|<=|>=|===|==|=~|!=|!~|(?=[\s\t])\?|
             (?<=[\s\t])!+|&(&&?|(?!\d))|\|\||\^|\*|\+|\-|/|
             \||\+\+|\-\-|\*\*|\/\/|\<\-|\<\>|<<|>>|=|\.|~~~}x, Operator
        rule %r{(?<!:)(:)([a-zA-Z_]\w*([?!]|=(?![>=]))?|\<\>|===?|>=?|<=?|
             <=>|&&?|%\(\)|%\[\]|%\{\}|\+\+?|\-\-?|\|\|?|\!|//|[%&`/\|]|
             \*\*?|=?~|<\-)|([a-zA-Z_]\w*([?!])?)(:)(?!:)}, Str::Symbol
        rule /:"/, Str::Symbol, :interpoling_symbol
        rule /\b(nil|true|false)\b(?![?!])|\b[A-Z]\w*\b/, Name::Constant
        rule /\b(__(FILE|LINE|MODULE|MAIN|FUNCTION)__)\b(?![?!])/, Name::Builtin::Pseudo
        rule /[a-zA-Z_!][\w_]*[!\?]?/, Name
        rule %r{::|[%(){};,/\|:\\\[\]]}, Punctuation
        rule /@[a-zA-Z_]\w*|&\d/, Name::Variable
        rule %r{\b(0[xX][0-9A-Fa-f]+|\d(_?\d)*(\.(?![^\d\s])
             (_?\d)*)?([eE][-+]?\d(_?\d)*)?|0[bB][01]+)\b}x, Num
        rule %r{~r\/.*\/}, Str::Regex

        mixin :strings
      end

      state :strings do
        rule /(%[A-Ba-z])?"""(?:.|\n)*?"""/, Str::Doc
        rule /'''(?:.|\n)*?'''/, Str::Doc
        rule /"/, Str::Doc, :dqs
        rule /'.*?'/, Str::Single
        rule %r{(?<!\w)\?(\\(x\d{1,2}|\h{1,2}(?!\h)\b|0[0-7]{0,2}(?![0-7])\b[^x0MC])|(\\[MC]-)+\w|[^\s\\])}, Str::Other

        BRACES.each do |_, _, name|
          mixin :"braces_#{name}"
        end
      end

      BRACES.each do |lbrace, rbrace, name|
        state :"braces_#{name}" do
          rule /%[a-z]#{lbrace}/, Str::Double, :"braces_#{name}_intp"
          rule /%[A-Z]#{lbrace}/, Str::Double, :"braces_#{name}_no_intp"
        end

        state :"braces_#{name}_intp" do
          rule /#{rbrace}[a-z]*/, Str::Double, :pop!
          mixin :enddoublestr
        end

        state :"braces_#{name}_no_intp" do
          rule /.*#{rbrace}[a-z]*/, Str::Double, :pop!
        end
      end

      state :dqs do
        rule /"/, Str::Double, :pop!
        mixin :enddoublestr
      end

      state :interpoling do
        rule /#\{/, Str::Interpol, :interpoling_string
      end

      state :interpoling_string do
        rule /\}/, Str::Interpol, :pop!
        mixin :root
      end

      state :interpoling_symbol do
        rule /"/, Str::Symbol, :pop!
        mixin :interpoling
        rule /[^#"]+/, Str::Symbol
      end

      state :enddoublestr do
        mixin :interpoling
        rule /[^#"]+/, Str::Double
      end
    end
  end
end
