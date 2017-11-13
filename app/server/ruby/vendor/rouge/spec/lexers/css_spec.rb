# -*- coding: utf-8 -*- #

describe Rouge::Lexers::CSS do
  let(:subject) { Rouge::Lexers::CSS.new }

  describe 'guessing' do
    include Support::Guessing
    it 'guesses by filename' do
      assert_guess :filename => 'foo.css'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/css'
    end
  end
end
