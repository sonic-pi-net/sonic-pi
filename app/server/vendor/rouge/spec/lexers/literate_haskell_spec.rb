# -*- coding: utf-8 -*- #

describe Rouge::Lexers::LiterateHaskell do
  let(:subject) { Rouge::Lexers::LiterateHaskell.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.lhs'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-literate-haskell'
    end
  end
end
