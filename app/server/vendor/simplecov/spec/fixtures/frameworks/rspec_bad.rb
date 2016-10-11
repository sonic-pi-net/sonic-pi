$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), "..", "..", ".."))
require "lib/simplecov"
require "rspec"
SimpleCov.start
describe "exit status" do
  it "should exit with a non-zero exit status when assertion fails" do
    expect(1).to eq(2)
  end
end
