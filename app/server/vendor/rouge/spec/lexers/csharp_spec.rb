# -*- coding: utf-8 -*- #

describe Rouge::Lexers::CSharp do
  let(:subject) { Rouge::Lexers::CSharp.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.cs'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-csharp'
    end
  end
end
