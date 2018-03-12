# -*- coding: utf-8 -*- #

describe Rouge::Lexers::SQL do
  let(:subject) { Rouge::Lexers::SQL.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.sql'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-sql'
    end
  end
end
