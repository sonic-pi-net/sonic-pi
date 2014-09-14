# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Slim do
  let(:subject) { Rouge::Lexers::Slim.new }
  include Support::Lexing

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.slim'
    end

    it 'guesses by source' do
      assert_guess :source => 'doctype html'

      assert_guess :source => <<-source
        html
          body
            p Some text
            a *{ :href => "Somewhere" } Link to somewhere
      source
    end
  end

end
