# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Scheme do
  let(:subject) { Rouge::Lexers::Scheme.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.scm'
      assert_guess :filename => 'foo.ss'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-scheme'
      assert_guess :mimetype => 'application/x-scheme'
    end
  end
end
