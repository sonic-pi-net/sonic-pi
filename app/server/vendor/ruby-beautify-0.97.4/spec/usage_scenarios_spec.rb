require 'spec_helper.rb'

describe "Usage Scenarios" do
	files = Dir.glob("spec/usage_scenarios/*_pretty.rb")
	scenarios = files.map do |f|
		r = /.*\/(?<scenario>.*)_pretty.rb/.match(f)
		r['scenario'] ||= nil
	end

	scenarios.each do |scenario|
		it "will test: #{scenario}" do
			scenario_file = "spec/usage_scenarios/#{scenario}.rb"
			scenario_md5_sum = Digest::MD5.hexdigest RubyBeautify.pretty_string File.read scenario_file

			scenario_pretty_file = "spec/usage_scenarios/#{scenario}_pretty.rb"
			scenario_pretty_md5_sum = Digest::MD5.hexdigest File.read(scenario_pretty_file)
			expect(scenario_md5_sum).to eq scenario_pretty_md5_sum
		end
	end
end
