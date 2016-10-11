require "helper"

# Test to verify correct handling of deleted files
# See https://github.com/colszowka/simplecov/issues/9
describe "A source file which is subsequently deleted" do
  it "does not cause an error" do
    Dir.chdir(File.join(File.dirname(__FILE__), "fixtures")) do
      `ruby deleted_source_sample.rb`
      expect($?.exitstatus).to be_zero
    end
  end
end
