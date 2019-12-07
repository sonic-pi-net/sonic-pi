# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Factor do
  let(:subject) { Rouge::Lexers::Factor.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.factor'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-factor'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/local/bin/factor'
    end
  end
end
