require File.dirname(__FILE__) + '/../../spec_helper.rb'

run_fixtures_for_language(:ruby)

describe 'Ruby' do

  describe 'matchers' do
    before(:each) do
      @ruby = RBeautify::Language.language(:ruby)
    end

    describe 'standard' do
      before(:each) do
        @matcher = @ruby.matcher(:standard)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, '', '')
      end

      it { @matcher.parse_block_start('class Foo; end', nil, 0, 0).should be_block_start_like(:standard, 0, 'class', ' Foo; end') }
      it { @matcher.parse_block_start('module Foo', nil, 0, 0).should be_block_start_like(:standard, 0, 'module', ' Foo') }
      it { @matcher.parse_block_start('def foo()', nil, 0, 0).should be_block_start_like(:standard, 0, 'def', ' foo()') }
      it { @matcher.parse_block_start('end Foo', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('end', 0).should be_block_end_like(@current_block, 0, 'end', '') }
      it { @current_block.parse_block_end(';end', 0).should be_block_end_like(@current_block, 0, ';end', '') }
      it { @current_block.parse_block_end('; end', 0).should be_block_end_like(@current_block, 1, ' end', '') }
      it { @current_block.parse_block_end('rescue', 0).should be_block_end_like(@current_block, 0, 'rescue', '') }
      it { @current_block.parse_block_end('ensure', 0).should be_block_end_like(@current_block, 0, 'ensure', '') }
      it { @current_block.parse_block_end('}', 0).should be_nil }
    end

    describe 'if' do
      before(:each) do
        @matcher = @ruby.matcher(:if)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, 'if', ' foo')
      end

      it { @matcher.should be_end_can_also_be_start}

      it { @matcher.parse_block_start('if foo', nil, 0, 0).should be_block_start_like(:if, 0, 'if', ' foo') }
      it { @matcher.parse_block_start('then foo = bar', nil, 0, 0).should be_block_start_like(:if, 0, 'then', ' foo = bar') }
      it { @matcher.parse_block_start('if_foo', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('end', 0).should be_block_end_like(@current_block, 0, 'end', '') }
      it { @current_block.parse_block_end('then', 0).should be_block_end_like(@current_block, 0, 'then', '') }
      it { @current_block.parse_block_end('else', 0).should be_block_end_like(@current_block, 0, 'else', '') }
      it { @current_block.parse_block_end('a = 3', 0).should be_nil }
    end

    describe 'curly_bracket' do
      before(:each) do
        @matcher = @ruby.matcher(:curly_bracket)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, '{', '')
      end

      it { @matcher.parse_block_start('{', nil, 0, 0).should be_block_start_like(:curly_bracket, 0, '{', '') }
      it { @matcher.parse_block_start('end', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('}', 0).should be_block_end_like(@current_block, 0, '}', '') }
      it { @current_block.parse_block_end('end', 0).should be_nil }

    end

    describe 'double_quote' do
      before(:each) do
        @matcher = @ruby.matcher(:double_quote)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, '"', 'foo"')
      end

      it { @matcher.parse_block_start('a = "foo"', nil, 0, 0).should be_block_start_like(:double_quote, 4, '"', 'foo"') }
      it { @matcher.parse_block_start('a = 2', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end(' bar"', 0).should be_block_end_like(@current_block, 4, '"', '') }
      it { @current_block.parse_block_end(' " + bar + "', 0).should be_block_end_like(@current_block, 1, '"', ' + bar + "') }
      it { @current_block.parse_block_end('\\\\"', 0).should be_block_end_like(@current_block, 2, '"', '') }
      it { @current_block.parse_block_end('\\" more string"', 0).should be_block_end_like(@current_block, 14, '"', '') }
      it { @current_block.parse_block_end('a = 2', 0).should be_nil }
      it { @current_block.parse_block_end('\\"', 0).should be_nil }
      it { @current_block.parse_block_end('\\\\\\"', 0).should be_nil }
    end

    describe 'single_quote' do
      before(:each) do
        @matcher = @ruby.matcher(:single_quote)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, "'", "foo'")
      end

      it { @matcher.should_not be_end_can_also_be_start}
      it { @matcher.parse_block_start("describe '#foo?' do", nil, 0, 0).should be_block_start_like(:single_quote, 9, "'", "#foo?' do") }
      it { @matcher.parse_block_start('a = 2', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end("#foo?' do", 9).should be_block_end_like(@current_block, 14, "'", ' do')}
      it { @current_block.parse_block_end('a = 2', 0).should be_nil }
    end

    describe 'continuing_line' do
      before(:each) do
        @matcher = @ruby.matcher(:continuing_line)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 8, ',', '')
      end

      it { @matcher.parse_block_start('foo :bar,', nil, 0, 0).should be_block_start_like(:continuing_line, 8, ',', '') }
      it { @matcher.parse_block_start('a = 3 &&', nil, 0, 0).should be_block_start_like(:continuing_line, 6, '&&', '') }
      it { @matcher.parse_block_start('a = 3 ||', nil, 0, 0).should be_block_start_like(:continuing_line, 6, '||', '') }
      it { @matcher.parse_block_start('a = 3 +', nil, 0, 0).should be_block_start_like(:continuing_line, 6, '+', '') }
      it { @matcher.parse_block_start('a = 3 -', nil, 0, 0).should be_block_start_like(:continuing_line, 6, '-', '') }
      it { @matcher.parse_block_start("a \\", nil, 0, 0).should be_block_start_like(:continuing_line, 2, '\\', '') }
      it { @matcher.parse_block_start('a ?', nil, 0, 0).should be_block_start_like(:continuing_line, 1, ' ?', '')  }
      it { @matcher.parse_block_start('a ? # some comment', nil, 0, 0).should be_block_start_like(:continuing_line, 1, ' ? # some comment', '') }
      it { @matcher.parse_block_start('a = 3', nil, 0, 0).should be_nil }
      it { @matcher.parse_block_start('a = foo.bar?', nil, 0, 0).should be_nil }
      it { @matcher.parse_block_start('# just a comment', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('a = 3', 0).should be_block_end_like(@current_block, 0, '', 'a = 3') }
      it { @current_block.parse_block_end('foo :bar,', 0).should be_nil }
      it { @current_block.parse_block_end('a = 3 &&', 0).should be_nil }
      it { @current_block.parse_block_end('a = 3 +', 0).should be_nil }
      it { @current_block.parse_block_end("a \\", 0).should be_nil }
      it { @current_block.parse_block_end('# just a comment', 0).should be_nil }
      it { @current_block.parse_block_end('#', 0).should be_nil }
    end

    describe 'round_bracket' do
      before(:each) do
        @matcher = @ruby.matcher(:round_bracket)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, '(', '')
      end

      it { @matcher.parse_block_start('a = (foo,', nil, 0, 0).should be_block_start_like(:round_bracket, 4, '(', 'foo,') }
      it { @matcher.parse_block_start('anything else', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('foo)', 0).should be_block_end_like(@current_block, 3, ')', '') }
      it { @current_block.parse_block_end('a = 3', 0).should be_nil }
    end

    describe 'comment' do
      before(:each) do
        @matcher = @ruby.matcher(:comment)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 8, '#', ' comment')
      end

      it { @matcher.parse_block_start('anything # comment', nil, 0, 0).should be_block_start_like(:comment, 8, ' #', ' comment') }
      it { @matcher.parse_block_start('#', nil, 0, 0).should be_block_start_like(:comment, 0, '#', '') }
      it { @matcher.parse_block_start('anything else', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('anything', 0).should be_block_end_like(@current_block, 8, '', '') }
      it { @current_block.parse_block_end('', 0).should be_block_end_like(@current_block, 0, '', '') }
    end

    describe 'begin' do
      before(:each) do
        @matcher = @ruby.matcher(:begin)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, 'begin', '')
      end

      it { @matcher.parse_block_start('begin', nil, 0, 0).should be_block_start_like(:begin, 0, 'begin', '') }
      it { @matcher.parse_block_start('beginning', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('rescue', 0).should be_block_end_like(@current_block, 0, 'rescue', '') }
      it { @current_block.parse_block_end('ensure', 0).should be_block_end_like(@current_block, 0, 'ensure', '') }
      it { @current_block.parse_block_end('else', 0).should be_block_end_like(@current_block, 0, 'else', '') }
      it { @current_block.parse_block_end('end', 0).should be_block_end_like(@current_block, 0, 'end', '') }
    end

    describe 'regex' do
      before(:each) do
        @matcher = @ruby.matcher(:regex)
        @current_block = RBeautify::BlockStart.new(@matcher, nil, 0, 0, '/', 'foo/')
      end

      it { @matcher.parse_block_start('/foo/', nil, 0, 0).should be_block_start_like(:regex, 0, '/', 'foo/') }
      it { @matcher.parse_block_start(', /foo/', nil, 0, 0).should be_block_start_like(:regex, 0, ', /', 'foo/') }
      it { @matcher.parse_block_start('foo = /bar/', nil, 0, 0).should be_block_start_like(:regex, 4, '= /', 'bar/') }
      it { @matcher.parse_block_start('foo =~ /bar/', nil, 0, 0).should be_block_start_like(:regex, 5, '~ /', 'bar/') }
      it { @matcher.parse_block_start('1/2', nil, 0, 0).should be_nil }
      it { @matcher.parse_block_start('anything else', nil, 0, 0).should be_nil }

      it { @current_block.parse_block_end('foo/', 0).should be_block_end_like(@current_block, 3, '/', '') }
      it { @current_block.parse_block_end('foo/', 0).should be_block_end_like(@current_block, 3, '/', '') }
      it { @current_block.parse_block_end('', 0).should be_nil }

    end
  end
end
