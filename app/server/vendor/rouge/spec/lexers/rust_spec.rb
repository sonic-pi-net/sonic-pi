# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Rust do
  let(:subject) { Rouge::Lexers::Rust.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.rs'
      assert_guess :filename => 'foo.rc'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-rust'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/bin/env rustc --jit'
    end
  end
end
