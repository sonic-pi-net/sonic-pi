# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Markdown do
  let(:subject) { Rouge::Lexers::Markdown.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.md'
      assert_guess :filename => 'foo.mkd'
      assert_guess :filename => 'foo.markdown'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-markdown'
    end
  end
end
