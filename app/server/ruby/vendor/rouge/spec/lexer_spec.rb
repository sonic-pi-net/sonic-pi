# -*- coding: utf-8 -*- #

describe Rouge::Lexer do
  include Support::Lexing

  it 'makes a simple lexer' do
    a_lexer = Class.new(Rouge::RegexLexer) do
      state :root do
        rule /a/, 'A'
        rule /b/, 'B'
      end
    end

    # consolidation
    result = a_lexer.lex('aa').to_a
    assert { result.size == 1 }
    assert { result == [['A', 'aa']] }

    result = a_lexer.lex('abab').to_a
    assert { result.size == 4 }
    assert { result == [['A', 'a'], ['B', 'b']] * 2 }
  end

  it 'pushes and pops states' do
    a_lexer = Class.new(Rouge::RegexLexer) do
      state :brace do
        rule /b/, 'B'
        rule /}/, 'Brace', :pop!
      end

      state :root do
        rule /{/, 'Brace', :brace
        rule /a/, 'A'
      end
    end

    result = a_lexer.lex('a{b}a').to_a
    assert { result.size == 5 }

    # failed parses

    t = Rouge::Token
    assert {
      a_lexer.lex('{a}').to_a ==
        [['Brace', '{'], [t['Error'], 'a'], ['Brace', '}']]
    }

    assert { a_lexer.lex('b').to_a == [[t['Error'], 'b']] }
    assert { a_lexer.lex('}').to_a == [[t['Error'], '}']] }
  end

  it 'does callbacks and grouping' do
    callback_lexer = Class.new(Rouge::RegexLexer) do
      state :root do
        rule /(a)(b)/ do |s|
          groups('A', 'B')
        end
      end
    end

    result = callback_lexer.lex('ab').to_a

    assert { result.size == 2 }
    assert { result[0] == ['A', 'a'] }
    assert { result[1] == ['B', 'b'] }
  end

  it 'pops from the callback' do
    callback_lexer = Class.new(Rouge::RegexLexer) do
      state :root do
        rule /a/, 'A', :a
        rule /d/, 'D'
      end

      state :a do
        rule /b/, 'B', :b
      end

      state :b do
        rule /c/ do |ss|
          token 'C'
          pop!; pop! # go back to the root
        end
      end
    end

    assert_no_errors 'abcd', callback_lexer
  end

  it 'supports stateful lexes' do
    stateful = Class.new(Rouge::RegexLexer) do
      def incr
        @count += 1
      end

      state :root do
        rule /\d+/ do |ss|
          token 'digit'
          @count = ss[0].to_i
        end

        rule /\+/ do |ss|
          incr
          token(@count <= 5 ? 'lt' : 'gt')
        end
      end
    end

    result = stateful.lex('4++')
    types = result.map { |(t,_)| t }
    assert { types == %w(digit lt gt) }
  end

  it 'delegates' do
    class MasterLexer < Rouge::RegexLexer
      state :root do
        rule /a/, 'A'
        rule /{(.*?)}/ do |m|
          token 'brace', '{'
          delegate BracesLexer.new, m[1]
          token 'brace', '}'
        end
      end
    end

    class BracesLexer < Rouge::RegexLexer
      state :root do
        rule /b/, 'B'
      end
    end

    assert_no_errors 'a{b}a', MasterLexer
  end

  it 'detects the beginnings of lines with ^ rules' do
    class MyLexer < Rouge::RegexLexer
      state :root do
        rule /^a/, 'start'
        rule /a/, 'not-start'
      end
    end

    assert_has_token('start', 'a', MyLexer)
    assert_has_token('start', "\na", MyLexer)
    deny_has_token('not-start', 'a', MyLexer)
    assert_has_token('not-start', 'aa', MyLexer)
  end
end
