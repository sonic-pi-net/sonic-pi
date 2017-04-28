#!/usr/bin/env rake
require "bundler/gem_tasks"

desc "Generate a new usage scenario."
task :generate_usage_scenario, [:scenario] do |task, args|
	unless args[:scenario]
		puts "must define a scenario"
		exit
	end
	files = ["#{args[:scenario]}.rb","#{args[:scenario]}_pretty.rb"]
	files.each do |f|
		f = "spec/usage_scenarios/#{f}"
		if File.exist? f
			puts "	skipping #{f}"
		else
			puts "	generating #{f}"
			FileUtils.touch f
		end
	end
end
