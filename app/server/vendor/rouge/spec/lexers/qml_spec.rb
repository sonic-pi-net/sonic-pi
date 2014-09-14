# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Qml do
  let(:subject) { Rouge::Lexers::Qml.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.qml'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'application/x-qml'
      assert_guess :mimetype => 'text/x-qml'
    end
  end
end
