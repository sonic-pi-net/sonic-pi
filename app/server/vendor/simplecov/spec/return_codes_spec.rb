require "helper"

# Make sure that exit codes of tests are propagated properly
# See https://github.com/colszowka/simplecov/issues/5
describe "return codes" do
  context "inside fixtures/frameworks" do
    around do |test|
      Dir.chdir(File.join(File.dirname(__FILE__), "fixtures", "frameworks")) do
        FileUtils.rm_rf("./coverage")
        test.call
      end
    end

    it "has return code 0 when running testunit_good.rb" do
      `ruby testunit_good.rb`
      expect($?.exitstatus).to be_zero
    end

    it "has return code 0 when running rspec_good.rb" do
      `rspec rspec_good.rb`
      expect($?.exitstatus).to be_zero
    end

    it "has non-0 return code when running testunit_bad.rb" do
      `ruby testunit_bad.rb`
      expect($?.exitstatus).not_to be_zero
    end

    it "has return code 1 when running rspec_bad.rb" do
      `rspec rspec_bad.rb`
      expect($?.exitstatus).not_to be_zero
    end
  end
end
