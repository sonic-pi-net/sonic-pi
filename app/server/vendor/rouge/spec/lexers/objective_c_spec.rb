# -*- coding: utf-8 -*- #

describe Rouge::Lexers::ObjectiveC do
  let(:subject) { Rouge::Lexers::ObjectiveC.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.m', :source => '@property'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-objective_c'
      assert_guess :mimetype => 'application/x-objective_c'
    end

    it 'guesses by source' do
      assert_guess :filename => 'foo.h', :source => '[foo bar: baz]'
      assert_guess :filename => 'foo.h', :source => '@"foo"'
      assert_guess :source => '@implementation Foo'
    end
  end
end

