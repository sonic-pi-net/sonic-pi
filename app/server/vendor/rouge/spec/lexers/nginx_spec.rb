# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Nginx do
  let(:subject) { Rouge::Lexers::Nginx.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'nginx.conf'
      deny_guess   :filename => 'foo.conf'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-nginx-conf'
    end
  end
end
