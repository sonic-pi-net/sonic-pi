# -*- coding: utf-8 -*- #

describe Rouge::Lexers::OCaml do
  let(:subject) { Rouge::Lexers::OCaml.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.ml'
      assert_guess :filename => 'foo.mli'
      assert_guess :filename => 'foo.mll'
      assert_guess :filename => 'foo.mly'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-ocaml'
    end
  end
end

