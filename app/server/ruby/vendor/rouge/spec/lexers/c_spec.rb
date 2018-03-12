# -*- coding: utf-8 -*- #

describe Rouge::Lexers::C do
  let(:subject) { Rouge::Lexers::C.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.c'
      assert_guess :filename => 'foo.h', :source => 'foo'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-csrc'
    end
  end
end
