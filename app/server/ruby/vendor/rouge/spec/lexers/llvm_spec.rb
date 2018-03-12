# -*- coding: utf-8 -*- #

describe Rouge::Lexers::LLVM do
  let(:subject) { Rouge::Lexers::LLVM.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.ll'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-llvm'
    end
  end
end
