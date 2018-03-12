# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Clojure do
  let(:subject) { Rouge::Lexers::Clojure.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.clj'
      assert_guess :filename => 'foo.cljs'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-clojure'
      assert_guess :mimetype => 'application/x-clojure'
    end
  end
end
