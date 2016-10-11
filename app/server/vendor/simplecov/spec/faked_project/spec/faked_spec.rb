require "spec_helper"

describe FakedProject do
  it "should return proper foo" do
    expect(FakedProject.foo).to eq("bar")
  end

  it "should test it's framework specific method" do
    expect(FrameworkSpecific.rspec).to eq("Only tested in RSpec")
  end
end
