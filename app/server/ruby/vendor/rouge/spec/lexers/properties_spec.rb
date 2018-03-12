# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Properties do
  let(:subject) { Rouge::Lexers::Properties.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.properties'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-java-properties'
    end
  end
end
