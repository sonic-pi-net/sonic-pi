# -*- coding: utf-8 -*- #

describe Rouge::Lexers::PlainText do
  let(:subject) { Rouge::Lexers::PlainText.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo'
      assert_guess :filename => 'foo.txt'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/plain'
      assert_guess :mimetype => 'text/x-something-else'
    end

    it 'guesses by source' do
      assert_guess :source => 'zorbl'
      assert_guess :source => '-:-:-:-:-'
    end
  end
end
