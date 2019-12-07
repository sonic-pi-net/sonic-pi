# -*- coding: utf-8 -*- #

describe Rouge::Lexers::TCL do
  let(:subject) { Rouge::Lexers::TCL.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.tcl'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-tcl'
      assert_guess :mimetype => 'application/x-tcl'
      assert_guess :mimetype => 'text/x-script.tcl'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/bin/env tclsh'
      assert_guess :source => '#!/usr/bin/jimsh'
      assert_guess :source => '#!/usr/bin/wish'
    end
  end
end
