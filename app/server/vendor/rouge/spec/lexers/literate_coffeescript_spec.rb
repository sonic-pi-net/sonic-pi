# -*- coding: utf-8 -*- #

describe Rouge::Lexers::LiterateCoffeescript do
  let(:subject) { Rouge::Lexers::LiterateCoffeescript.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.litcoffee'
    end
  end
end
