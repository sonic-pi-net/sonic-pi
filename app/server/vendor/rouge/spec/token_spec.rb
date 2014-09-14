# -*- coding: utf-8 -*- #

describe Rouge::Token do
  it 'has a name' do
    assert { Rouge::Token['Text'].qualname == 'Text' }
    assert { Rouge::Token['Literal.String'].qualname == 'Literal.String' }
  end

  it 'has shortnames' do
    assert { Rouge::Token['Name'].shortname == 'n' }
    assert { Rouge::Token['Literal.String.Backtick'].shortname == 'sb' }
  end

  it 'calculates ancestors' do
    chain = Rouge::Token['Literal.String.Backtick'].token_chain.map(&:qualname)

    assert { chain == %w(Literal Literal.String Literal.String.Backtick) }
  end
end
