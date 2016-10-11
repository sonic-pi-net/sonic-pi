require "helper"

# Tests that verify that on 1.8 versions of ruby, simplecov simply
# does not launch and does not cause errors on the way
#
# TODO: This should be expanded upon all methods that could potentially
# be called in a test/spec-helper simplecov config block
#
describe "Ruby 1.8 fallback" do
  it "return false when calling SimpleCov.start" do
    expect(SimpleCov.start).to be false
  end

  it "return false when calling SimpleCov.start with a block" do
    expect(SimpleCov.start { raise "Shouldn't reach this!" }).to be false
  end

  it "return false when calling SimpleCov.configure with a block" do
    expect(SimpleCov.configure { raise "Shouldn't reach this!" }).to be false
  end

  it "allow to define a profile" do
    expect do
      SimpleCov.profiles.define "testprofile" do
        add_filter "/config/"
      end
    end.not_to raise_error
  end
end if RUBY_VERSION.start_with? "1.8"
