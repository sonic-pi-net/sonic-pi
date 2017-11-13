# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Make do
  let(:subject) { Rouge::Lexers::Make.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.make'
      assert_guess :filename => 'Makefile'
      assert_guess :filename => 'makefile'
      assert_guess :filename => 'Makefile.in'
      assert_guess :filename => 'GNUmakefile'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-makefile'
    end

    it 'guesses by source' do
      assert_guess :source => '.PHONY: all'
    end
  end
end
