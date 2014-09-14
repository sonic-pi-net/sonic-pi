# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Coffeescript do
  let(:subject) { Rouge::Lexers::Coffeescript.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.coffee'
      assert_guess :filename => 'Cakefile'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/coffeescript'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/bin/env coffee'
    end
  end
end
