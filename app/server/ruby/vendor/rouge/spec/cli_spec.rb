# -*- coding: utf-8 -*- #

require 'rouge/cli'

describe Rouge::CLI do
  let(:argv) { [] }
  subject { Rouge::CLI.parse(argv) }

  describe Rouge::CLI::Help do
    describe '-h' do
      let(:argv) { %w(-h) }
      it('parses') { assert { Rouge::CLI::Help === subject } }
    end

    describe '--help' do
      let(:argv) { %w(--help) }
      it('parses') { assert { Rouge::CLI::Help === subject } }
    end

    describe 'help' do
      let(:argv) { %w(help) }
      it('parses') { assert { Rouge::CLI::Help === subject } }
    end
  end

  describe Rouge::CLI::Highlight do
    describe 'specifying a lexer' do
      let(:argv) { %w(highlight -l ruby) }
      it('parses') {
        assert { Rouge::CLI::Highlight === subject }
        assert { Rouge::Lexers::Ruby === subject.lexer }
      }
    end

    describe 'guessing a lexer by mimetype' do
      let(:argv) { %w(highlight -m application/javascript) }
      it('parses') {
        assert { Rouge::Lexers::Javascript === subject.lexer }
      }
    end

    describe 'guessing a lexer by file contents' do
      let(:argv) { %w(highlight -i bin/rougify) }
      it('parses') {
        assert { Rouge::Lexers::Ruby === subject.lexer }
      }
    end
  end

  describe Rouge::CLI::List do
    describe 'list' do
      let(:argv) { %w(list) }
      it('parses') { assert { Rouge::CLI::List === subject } }

      it 'lists available lexers' do
        out, err = capture_io { subject.run }

        expected_tags = Rouge::Lexer.all.map(&:tag).sort
        actual_tags = out.scan(/^(.*?):/).flatten

        assert_equal expected_tags, actual_tags, "err: #{err.inspect}"
      end
    end
  end
end
