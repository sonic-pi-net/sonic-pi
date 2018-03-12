# -*- coding: utf-8 -*- #

module Rouge
  module Lexers
    class Javascript < RegexLexer
      desc "JavaScript, the browser scripting language"

      tag 'javascript'
      aliases 'js'
      filenames '*.js'
      mimetypes 'application/javascript', 'application/x-javascript',
                'text/javascript', 'text/x-javascript'

      def self.analyze_text(text)
        return 1 if text.shebang?('node')
        return 1 if text.shebang?('jsc')
        # TODO: rhino, spidermonkey, etc
      end

      state :comments_and_whitespace do
        rule /\s+/, Text
        rule /<!--/, Comment # really...?
        rule %r(//.*?$), Comment::Single
        rule %r(/\*.*?\*/)m, Comment::Multiline
      end

      state :expr_start do
        mixin :comments_and_whitespace

        rule %r(/) do
          token Str::Regex
          goto :regex
        end

        rule /[{]/, Punctuation, :object

        rule //, Text, :pop!
      end

      state :regex do
        rule %r(/) do
          token Str::Regex
          goto :regex_end
        end

        rule %r([^/]\n), Error, :pop!

        rule /\n/, Error, :pop!
        rule /\[\^/, Str::Escape, :regex_group
        rule /\[/, Str::Escape, :regex_group
        rule /\\./, Str::Escape
        rule %r{[(][?][:=<!]}, Str::Escape
        rule /[{][\d,]+[}]/, Str::Escape
        rule /[()?]/, Str::Escape
        rule /./, Str::Regex
      end

      state :regex_end do
        rule /[gim]+/, Str::Regex, :pop!
        rule(//) { pop! }
      end

      state :regex_group do
        # specially highlight / in a group to indicate that it doesn't
        # close the regex
        rule /\//, Str::Escape

        rule %r([^/]\n) do
          token Error
          pop! 2
        end

        rule /\]/, Str::Escape, :pop!
        rule /\\./, Str::Escape
        rule /./, Str::Regex
      end

      state :bad_regex do
        rule /[^\n]+/, Error, :pop!
      end

      def self.keywords
        @keywords ||= Set.new %w(
          for in while do break return continue switch case default
          if else throw try catch finally new delete typeof instanceof
          void this
        )
      end

      def self.declarations
        @declarations ||= Set.new %w(var let with function)
      end

      def self.reserved
        @reserved ||= Set.new %w(
          abstract boolean byte char class const debugger double enum
          export extends final float goto implements import int interface
          long native package private protected public short static
          super synchronized throws transient volatile
        )
      end

      def self.constants
        @constants ||= Set.new %w(true false null NaN Infinity undefined)
      end

      def self.builtins
        @builtins ||= %w(
          Array Boolean Date Error Function Math netscape
          Number Object Packages RegExp String sun decodeURI
          decodeURIComponent encodeURI encodeURIComponent
          Error eval isFinite isNaN parseFloat parseInt document this
          window
        )
      end

      id = /[$a-zA-Z_][a-zA-Z0-9_]*/

      state :root do
        rule /\A\s*#!.*?\n/m, Comment::Preproc, :statement
        rule /\n/, Text, :statement
        rule %r((?<=\n)(?=\s|/|<!--)), Text, :expr_start
        mixin :comments_and_whitespace
        rule %r(\+\+ | -- | ~ | && | \|\| | \\(?=\n) | << | >>>? | ===
               | !== )x,
          Operator, :expr_start
        rule %r([-<>+*%&|\^/!=]=?), Operator, :expr_start
        rule /[(\[,]/, Punctuation, :expr_start
        rule /;/, Punctuation, :statement
        rule /[)\].]/, Punctuation

        rule /[?]/ do
          token Punctuation
          push :ternary
          push :expr_start
        end

        rule /[{}]/, Punctuation, :statement

        rule id do |m|
          if self.class.keywords.include? m[0]
            token Keyword
            push :expr_start
          elsif self.class.declarations.include? m[0]
            token Keyword::Declaration
            push :expr_start
          elsif self.class.reserved.include? m[0]
            token Keyword::Reserved
          elsif self.class.constants.include? m[0]
            token Keyword::Constant
          elsif self.class.builtins.include? m[0]
            token Name::Builtin
          else
            token Name::Other
          end
        end

        rule /[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?/, Num::Float
        rule /0x[0-9a-fA-F]+/, Num::Hex
        rule /[0-9]+/, Num::Integer
        rule /"(\\\\|\\"|[^"])*"/, Str::Double
        rule /'(\\\\|\\'|[^'])*'/, Str::Single
      end

      # braced parts that aren't object literals
      state :statement do
        rule /(#{id})(\s*)(:)/ do
          groups Name::Label, Text, Punctuation
        end

        mixin :expr_start
      end

      # object literals
      state :object do
        mixin :comments_and_whitespace
        rule /[}]/ do
          token Punctuation
          goto :statement
        end

        rule /(#{id})(\s*)(:)/ do
          groups Name::Attribute, Text, Punctuation
          push :expr_start
        end

        rule /:/, Punctuation
        mixin :root
      end

      # ternary expressions, where <id>: is not a label!
      state :ternary do
        rule /:/ do
          token Punctuation
          goto :expr_start
        end

        mixin :root
      end
    end

    class JSON < RegexLexer
      desc "JavaScript Object Notation (json.org)"
      tag 'json'
      filenames '*.json'
      mimetypes 'application/json'

      # TODO: is this too much of a performance hit?  JSON is quite simple,
      # so I'd think this wouldn't be too bad, but for large documents this
      # could mean doing two full lexes.
      def self.analyze_text(text)
        return 0.8 if text =~ /\A\s*{/m && text.lexes_cleanly?(self)
      end

      state :root do
        mixin :whitespace
        # special case for empty objects
        rule /(\{)(\s*)(\})/m do
          groups Punctuation, Text::Whitespace, Punctuation
        end
        rule /(?:true|false|null)\b/, Keyword::Constant
        rule /{/,  Punctuation, :object_key
        rule /\[/, Punctuation, :array
        rule /-?(?:0|[1-9]\d*)\.\d+(?:e[+-]\d+)?/i, Num::Float
        rule /-?(?:0|[1-9]\d*)(?:e[+-]\d+)?/i, Num::Integer
        mixin :has_string
      end

      state :whitespace do
        rule /\s+/m, Text::Whitespace
      end

      state :has_string do
        rule /"(\\.|[^"])*"/, Str::Double
      end

      state :object_key do
        mixin :whitespace
        mixin :has_string
        rule /:/, Punctuation, :object_val
        rule /}/, Error, :pop!
      end

      state :object_val do
        rule /,/, Punctuation, :pop!
        rule(/}/) { token Punctuation; pop!(2) }
        mixin :root
      end

      state :array do
        rule /\]/, Punctuation, :pop!
        rule /,/, Punctuation
        mixin :root
      end
    end
  end
end
