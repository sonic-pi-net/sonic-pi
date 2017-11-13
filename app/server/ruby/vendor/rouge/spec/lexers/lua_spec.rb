# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Lua do
  let(:subject) { Rouge::Lexers::Lua.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.lua'
      assert_guess :filename => 'foo.wlua'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-lua'
      assert_guess :mimetype => 'application/x-lua'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/local/bin/lua'
    end
  end
end
