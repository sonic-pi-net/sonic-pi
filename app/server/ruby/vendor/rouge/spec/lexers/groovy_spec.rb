# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Groovy do
  let(:subject) { Rouge::Lexers::Groovy.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.groovy'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-groovy'
    end
  end
end
