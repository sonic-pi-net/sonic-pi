# -*- coding: utf-8 -*- #

# stdlib
require 'pathname'

# The containing module for Rouge
module Rouge
  class << self
    # Highlight some text with a given lexer and formatter.
    #
    # @example
    #   Rouge.highlight('@foo = 1', 'ruby', 'html')
    #   Rouge.highlight('var foo = 1;', 'js', 'terminal256')
    #
    #   # streaming - chunks become available as they are lexed
    #   Rouge.highlight(large_string, 'ruby', 'html') do |chunk|
    #     $stdout.print chunk
    #   end
    def highlight(text, lexer, formatter, &b)
      lexer = Lexer.find(lexer) unless lexer.respond_to? :lex
      raise "unknown lexer #{lexer}" unless lexer

      formatter = Formatter.find(formatter) unless formatter.respond_to? :format
      raise "unknown formatter #{formatter}" unless formatter

      formatter.format(lexer.lex(text), &b)
    end
  end
end

load_dir = Pathname.new(__FILE__).dirname
load load_dir.join('rouge/version.rb')

load load_dir.join('rouge/util.rb')

load load_dir.join('rouge/text_analyzer.rb')
load load_dir.join('rouge/token.rb')

load load_dir.join('rouge/lexer.rb')
load load_dir.join('rouge/regex_lexer.rb')
load load_dir.join('rouge/template_lexer.rb')

Dir.glob(load_dir.join('rouge/lexers/*.rb')).each { |f| load f }

load load_dir.join('rouge/formatter.rb')
load load_dir.join('rouge/formatters/html.rb')
load load_dir.join('rouge/formatters/terminal256.rb')
load load_dir.join('rouge/formatters/null.rb')

load load_dir.join('rouge/theme.rb')
load load_dir.join('rouge/themes/thankful_eyes.rb')
load load_dir.join('rouge/themes/colorful.rb')
load load_dir.join('rouge/themes/base16.rb')
load load_dir.join('rouge/themes/github.rb')
load load_dir.join('rouge/themes/monokai.rb')
load load_dir.join('rouge/themes/monokai_sublime.rb')
