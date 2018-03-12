# -*- coding: utf-8 -*- #

describe Rouge::Lexers::R do
  let(:subject) { Rouge::Lexers::R.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.R'
      assert_guess :filename => 'foo.S'
      assert_guess :filename => '.Rhistory'
      assert_guess :filename => '.Rprofile'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-r'
      assert_guess :mimetype => 'application/x-r'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/bin/Rscript'
    end
  end
end

