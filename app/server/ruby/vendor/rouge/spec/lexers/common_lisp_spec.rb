# -*- coding: utf-8 -*- #

describe Rouge::Lexers::CommonLisp do
  let(:subject) { Rouge::Lexers::CommonLisp.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.cl'
      assert_guess :filename => 'foo.lisp'
      assert_guess :filename => 'foo.el'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-common-lisp'
    end
  end
end
