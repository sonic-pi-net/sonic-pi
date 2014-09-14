# -*- coding: utf-8 -*- #

describe Rouge::Lexers do
  spec_dir = Pathname.new(__FILE__).dirname
  samples_dir = spec_dir.join('visual/samples')

  Rouge::Lexer.all.each do |lexer_class|
    describe lexer_class do
      include Support::Lexing

      subject { lexer_class.new }

      it 'lexes the demo with no errors' do
        assert_no_errors(lexer_class.demo)
      end

      it 'lexes the sample without throwing' do
        sample = File.read(samples_dir.join(lexer_class.tag), encoding: 'utf-8')
        subject.lex(sample).to_a
      end
    end
  end
end
