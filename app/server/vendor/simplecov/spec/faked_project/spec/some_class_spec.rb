require "spec_helper"

describe SomeClass do
  subject { SomeClass.new("foo") }

  it "should be reversible" do
    expect(subject.reverse).to eq("oof")
  end

  it "should compare with 'foo'" do
    expect(subject.compare_with("foo")).to be true
  end
end
