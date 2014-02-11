require "spec_helper"
require "hamster/immutable"

describe Hamster::Immutable do
  class NewPerson < Struct.new(:first, :last)
    include Hamster::Immutable
  end

  let(:immutable) { NewPerson.new("Simon", "Harris") }

  it "freezes the instance" do
    expect(immutable).to be_frozen
  end
end
