# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Conf do
  let(:subject) { Rouge::Lexers::Conf.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess   :filename => 'foo.conf'

      # this should be lexed with the special nginx lexer
      # instead.
      deny_guess :filename => 'nginx.conf'
    end
  end
end
