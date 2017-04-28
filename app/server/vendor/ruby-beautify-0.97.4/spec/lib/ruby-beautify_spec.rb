require 'spec_helper.rb'

# unit tests suck.  But I do want to make sure this one function behaves as expected.
# it simplifies the rest of the testing process (by not requiring multiple passes of each scenario).
describe "Ruby Beautify Library" do
	before :all do
		@ugly_string = File.open('spec/usage_scenarios/pre_indented.rb').read
		@pretty_string = File.open('spec/usage_scenarios/pre_indented_pretty.rb').read
		@pretty_string_tabs = @pretty_string.gsub("\t", "\t\t")
		@pretty_string_space = File.open('spec/usage_scenarios/pre_indented_pretty_space.rb').read
	end

	describe "#pretty_string" do
		it "will indent by tab ( by default )" do
			expect(RubyBeautify.pretty_string(@ugly_string)).to eq @pretty_string
		end
		it "will indent by space" do
			expect(RubyBeautify.pretty_string(@ugly_string, indent_token:' ')).to eq @pretty_string_space
		end
		it "will honor indent count" do
			expect(RubyBeautify.pretty_string(@ugly_string)).to eq @pretty_string
		end
	end
end
