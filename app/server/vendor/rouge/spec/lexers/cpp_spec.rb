# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Cpp do
  let(:subject) { Rouge::Lexers::Cpp.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.cpp'
      assert_guess :filename => 'foo.c++'
      assert_guess :filename => 'foo.cc'
      assert_guess :filename => 'foo.cxx'

      assert_guess :filename => 'foo.hpp'
      assert_guess :filename => 'foo.h++'
      assert_guess :filename => 'foo.hh'
      assert_guess :filename => 'foo.hxx'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-c++hdr'
      assert_guess :mimetype => 'text/x-c++src'
    end
  end
end
