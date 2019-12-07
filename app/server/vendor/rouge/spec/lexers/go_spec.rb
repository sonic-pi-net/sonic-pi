# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Go do
  let(:subject) { Rouge::Lexers::Go.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.go'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-go'
      assert_guess :mimetype => 'application/x-go'
    end
  end
end

