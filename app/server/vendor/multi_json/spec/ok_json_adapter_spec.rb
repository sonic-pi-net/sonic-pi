require 'spec_helper'
require 'shared/adapter'
require 'multi_json/adapters/ok_json'

describe MultiJson::Adapters::OkJson do
  it_behaves_like 'an adapter', described_class
end