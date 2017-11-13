# -*- coding: utf-8 -*- #

describe Rouge::Lexers::Puppet do
  let(:subject) { Rouge::Lexers::Puppet.new }

  describe 'guessing' do
    include Support::Guessing

    it 'guesses by filename' do
      assert_guess :filename => 'foo.pp'
    end

    it 'guesses by source' do
      assert_guess :source => '#!/usr/local/bin/puppet apply'
      assert_guess :source => '#!/usr/bin/puppet-apply'
    end
  end
end
