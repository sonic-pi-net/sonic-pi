# -*- coding: utf-8 -*- #

describe Rouge::Formatters::Null do
  let(:subject) { Rouge::Formatters::Null.new }

  it 'renders nothing' do
    result = subject.format([[Token['Text'], 'foo']])

    assert { result == '' }
  end

  it 'consumes tokens' do
    consumed = false
    tokens = Enumerator.new { consumed = true }

    subject.format(tokens)

    assert consumed
  end
end
