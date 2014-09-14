# -*- coding: utf-8 -*- #

describe Rouge::Lexers::INI do
  let(:subject) { Rouge::Lexers::INI.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.ini'
      assert_guess :filename => '.gitconfig'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-ini'
    end
  end
end
