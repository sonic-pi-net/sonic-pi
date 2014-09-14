# -*- coding: utf-8 -*- #

describe Rouge::Lexers::VimL do
  let(:subject) { Rouge::Lexers::VimL.new }

  describe 'mapping binary search' do
    let(:mapping) {
      [['aa', 'aaa'],
       ['bb', 'bb'],
       ['cc', 'ccc']]
    }

    it 'finds a likely mapping' do
      likely = subject.find_likely_mapping(mapping, 'aa')
      assert { likely == ['aa', 'aaa'] }

      likely = subject.find_likely_mapping(mapping, 'abc')
      assert { likely == ['aa', 'aaa'] }

      likely = subject.find_likely_mapping(mapping, 'bbbbb')
      assert { likely == ['bb', 'bb'] }

      likely = subject.find_likely_mapping(mapping, 'z')
      assert { likely == ['cc', 'ccc'] }
    end
  end

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.vim'
      assert_guess :filename => 'foo.vba'
      assert_guess :filename => '~/.vimrc'
      assert_guess :filename => '.exrc'
      assert_guess :filename => '.gvimrc'
      assert_guess :filename => '_vimrc'
      assert_guess :filename => '_gvimrc'
      assert_guess :filename => '_exrc'
    end

    it 'guesses by mimetype' do
      assert_guess :mimetype => 'text/x-vim'
    end
  end
end
