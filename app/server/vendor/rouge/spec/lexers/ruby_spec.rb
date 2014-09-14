# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Ruby do
  let(:subject) { Rouge::Lexers::Ruby.new }

  describe 'lexing' do
    include Support::Lexing

    describe 'method calling' do
      describe 'leading dot' do
        it 'handles whitespace between the receiver and the method' do
          assert_tokens_equal "foo\n  .bar()",
            ['Name', 'foo'],
            ['Text', "\n  "],
            ['Punctuation', '.'],
            ['Name.Function', 'bar'],
            ['Punctuation', '()']
        end

        it 'handles whitespace between the receiver and multiple chained methods' do
          assert_tokens_equal "foo\n  .bar()\n  .baz",
            ['Name', 'foo'],
            ['Text', "\n  "],
            ['Punctuation', '.'],
            ['Name.Function', 'bar'],
            ['Punctuation', '()'],
            ['Text', "\n  "],
            ['Punctuation', '.'],
            ['Name.Function', 'baz']
        end
      end

      describe 'trailing dot' do
        it 'handles whitespace between the receiver and the method' do
          assert_tokens_equal "foo.\n  bar()",
            ['Name', 'foo'],
            ['Punctuation', '.'],
            ['Text', "\n  "],
            ['Name.Function', 'bar'],
            ['Punctuation', '()']
        end

        it 'handles whitespace between the receiver and multiple chained methods' do
          assert_tokens_equal "foo.\n  bar().\n  baz",
            ['Name', 'foo'],
            ['Punctuation', '.'],
            ['Text', "\n  "],
            ['Name.Function', 'bar'],
            ['Punctuation', '().'],
            ['Text', "\n  "],
            ['Name.Function', 'baz']
        end
      end
    end
  end

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.rb'
      assert_guess :filename => 'foo.ruby'
      assert_guess :filename => 'foo.rbw'
      assert_guess :filename => 'foo.gemspec'
      assert_guess :filename => 'Rakefile'
      assert_guess :filename => 'Guardfile'
      assert_guess :filename => 'Gemfile'
      assert_guess :filename => 'foo.rake'
      assert_guess :filename => 'Capfile'
      assert_guess :filename => 'Vagrantfile'
      assert_guess :filename => 'config.ru'
      assert_guess :filename => 'foo.pdf.prawn'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-ruby'
      assert_guess :mimetype => 'application/x-ruby'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/local/bin/ruby'
    end
  end
end
