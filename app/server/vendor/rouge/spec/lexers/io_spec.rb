# -*- coding: utf-8 -*- #

describe Rouge::Lexers::IO do
  let(:subject) { Rouge::Lexers::IO.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.io'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-iosrc'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/local/bin/io'
    end
  end
end
