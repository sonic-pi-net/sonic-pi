# -*- coding: utf-8 -*- #

describe Rouge::Formatters::HTML do
  let(:subject) { Rouge::Formatters::HTML.new(options) }
  let(:options) { {} }
  Token = Rouge::Token

  it 'formats a simple token stream' do
    out = subject.format([[Token['Name'], 'foo']])
    assert { out == %(<pre><code class="highlight"><span class="n">foo</span></code></pre>\n) }
  end

  describe 'skipping the wrapper' do
    let(:options) { { :wrap => false } }
    let(:output) { subject.format([[Token['Name'], 'foo']]) }

    it 'skips the wrapper' do
      assert { output == '<span class="n">foo</span>' }
    end
  end

  describe '#inline_theme' do
    class InlineTheme < Rouge::CSSTheme
      style Name, :bold => true
    end

    let(:options) { { :inline_theme => InlineTheme.new, :wrap => false } }

    let(:output) {
      subject.format([[Token['Name'], 'foo']])
    }

    it 'inlines styles given a theme' do
      assert { output == '<span style="font-weight: bold">foo</span>' }
    end
  end

  describe 'tableized line numbers' do
    let(:options) { { :line_numbers => true } }

    let(:text) { Rouge::Lexers::Clojure.demo }
    let(:tokens) { Rouge::Lexers::Clojure.lex(text) }

    let(:output) { subject.format(tokens) }
    let(:line_numbers) { output[%r[<pre class="lineno".*?</pre>]m].scan(/\d+/m).size }

    let(:output_code) {
      output =~ %r(<td class="code">(.*?)</td>)m
      $1
    }

    let(:code_lines) { output_code.scan(/\n/).size }

    it 'preserves the number of lines' do
      assert { code_lines == line_numbers }
    end
  end
end
