# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    class PHP < TemplateLexer
      desc "The PHP scripting language (php.net)"
      tag 'php'
      aliases 'php', 'php3', 'php4', 'php5'
      filenames '*.php', '*.php[345]'
      mimetypes 'text/x-php'

      default_options :parent => 'html'

      def initialize(opts={})
        # if truthy, the lexer starts highlighting with php code
        # (no <?php required)
        @start_inline = opts.delete(:start_inline)
        @funcnamehighlighting = opts.delete(:funcnamehighlighting) { true }
        @disabledmodules = opts.delete(:disabledmodules) { [] }

        super(opts)
      end

      def self.builtins
        load Pathname.new(__FILE__).dirname.join('php/builtins.rb')
        self.builtins
      end

      def builtins
        return [] unless @funcnamehighlighting

        @builtins ||= Set.new.tap do |builtins|
          self.class.builtins.each do |mod, fns|
            next if @disabledmodules.include? mod
            builtins.merge(fns)
          end
        end
      end

      def start_inline?
        !!@start_inline
      end

      start do
        push :php if start_inline?
      end

      def self.keywords
        @keywords ||= Set.new %w(
          and E_PARSE old_function E_ERROR or as E_WARNING parent eval
          PHP_OS break exit case extends PHP_VERSION cfunction FALSE
          print for require continue foreach require_once declare return
          default static do switch die stdClass echo else TRUE elseif
          var empty if xor enddeclare include virtual endfor include_once
          while endforeach global __FILE__ endif list __LINE__ endswitch
          new __sleep endwhile not array __wakeup E_ALL NULL final
          php_user_filter interface implements public private protected
          abstract clone try catch throw this use namespace
        )
      end

      state :root do
        rule /<\?(php|=)?/, Comment::Preproc, :php
        rule(/.*?(?=<\?)|.*/m) { delegate parent }
      end

      state :php do
        rule /\?>/, Comment::Preproc, :pop!
        # heredocs
        rule /<<<('?)([a-z_]\w*)\1\n.*?\n\2;?\n/im, Str::Heredoc
        rule /\s+/, Text
        rule /#.*?\n/, Comment::Single
        rule %r(//.*?\n), Comment::Single
        # empty comment, otherwise seen as the start of a docstring
        rule %r(/\*\*/), Comment::Multiline
        rule %r(/\*\*.*?\*/)m, Str::Doc
        rule %r(/\*.*?\*/)m, Comment::Multiline
        rule /(->|::)(\s*)([a-zA-Z_][a-zA-Z0-9_]*)/ do
          groups Operator, Text, Name::Attribute
        end

        rule /[~!%^&*+=\|:.<>\/?@-]+/, Operator
        rule /[\[\]{}();,]+/, Punctuation
        rule /class\b/, Keyword, :classname
        # anonymous functions
        rule /(function)(\s*)(?=\()/ do
          groups Keyword, Text
        end

        # named functions
        rule /(function)(\s+)(&?)(\s*)/ do
          groups Keyword, Text, Operator, Text
          push :funcname
        end

        rule /(const)(\s+)([a-zA-Z_]\w*)/i do
          groups Keyword, Text, Name::Constant
        end

        rule /(true|false|null)\b/, Keyword::Constant
        rule /\$\{\$+[a-z_]\w*\}/i, Name::Variable
        rule /\$+[a-z_]\w*/i, Name::Variable

        # may be intercepted for builtin highlighting
        rule /[\\a-z_][\\\w]*/i do |m|
          name = m[0]

          if self.class.keywords.include? name
            token Keyword
          elsif self.builtins.include? name
            token Name::Builtin
          else
            token Name::Other
          end
        end

        rule /(\d+\.\d*|\d*\.\d+)(e[+-]?\d+)?/i, Num::Float
        rule /\d+e[+-]?\d+/i, Num::Float
        rule /0[0-7]+/, Num::Oct
        rule /0x[a-f0-9]+/i, Num::Hex
        rule /\d+/, Num::Integer
        rule /'([^'\\]*(?:\\.[^'\\]*)*)'/, Str::Single
        rule /`([^`\\]*(?:\\.[^`\\]*)*)`/, Str::Backtick
        rule /"/, Str::Double, :string
      end

      state :classname do
        rule /\s+/, Text
        rule /[a-z_][\\\w]*/i, Name::Class, :pop!
      end

      state :funcname do
        rule /[a-z_]\w*/i, Name::Function, :pop!
      end

      state :string do
        rule /"/, Str::Double, :pop!
        rule /[^\\{$"]+/, Str::Double
        rule /\\([nrt\"$\\]|[0-7]{1,3}|x[0-9A-Fa-f]{1,2})/,
          Str::Escape
        rule /\$[a-zA-Z_][a-zA-Z0-9_]*(\[\S+\]|->[a-zA-Z_][a-zA-Z0-9_]*)?/, Name::Variable

        rule /\{\$\{/, Str::Interpol, :interp_double
        rule /\{(?=\$)/, Str::Interpol, :interp_single
        rule /(\{)(\S+)(\})/ do
          groups Str::Interpol, Name::Variable, Str::Interpol
        end

        rule /[${\\]+/, Str::Double
      end

      state :interp_double do
        rule /\}\}/, Str::Interpol, :pop!
        mixin :php
      end

      state :interp_single do
        rule /\}/, Str::Interpol, :pop!
        mixin :php
      end
    end
  end
end
