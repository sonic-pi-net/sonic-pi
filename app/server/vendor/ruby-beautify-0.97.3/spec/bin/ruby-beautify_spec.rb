require 'spec_helper.rb'

describe "Ruby Beautify" do
	# This acts as a config block for files and anything else that you may set.
	before (:all) do
		# a very quick to parse file, since this isn't to test the function of the parser (but the function of the binary).
		@small_file = 'spec/binary_scenarios/small_example.rb'
		# should be the contents of the small file twice.
		@doubled_file = 'spec/binary_scenarios/doubled_example.rb'
		# Our file to overwrite (should be ugly to start).
		@overwrite_file = "spec/binary_scenarios/overwrite.rb"
		@overwrite_target_file = "tmp/copied.rb"
		@overwrite_pretty_file = "spec/binary_scenarios/overwrite_pretty.rb"
		# Our space test.
		@space_before = 'spec/binary_scenarios/spaces_before.rb'
		@space_after = 'spec/binary_scenarios/spaces_after.rb'
		@space_after_sum = Digest::MD5.hexdigest File.read @space_after
		# our count test.
		@count_before = 'spec/binary_scenarios/count_before.rb'
		@count_after = 'spec/binary_scenarios/count_after.rb'
	end

	it "will work" do
		small_md5_sum = Digest::MD5.hexdigest File.read(@small_file)
		md5_sum = Digest::MD5.hexdigest `bundle exec #{BEAUTIFY_BIN} #{@small_file}`
		expect(md5_sum).to eq small_md5_sum
	end

	it "will do multiple files" do
		md5_sum = Digest::MD5.hexdigest `bundle exec #{BEAUTIFY_BIN} #{@small_file} #{@small_file}`
		doubled_md5_sum = Digest::MD5.hexdigest File.read(@doubled_file)
		expect(md5_sum).to eq doubled_md5_sum
	end

	# I want to make sure the file actually changes, so I do this (I could make yet another file).
	it "will update files (overwrite) in place" do
		FileUtils.mkdir_p File.dirname @overwrite_target_file
		FileUtils.cp @overwrite_file, @overwrite_target_file
		`bundle exec #{BEAUTIFY_BIN} --overwrite #{@overwrite_target_file}`
		md5_sum = Digest::MD5.hexdigest File.read @overwrite_target_file
	 	overwrite_md5_sum = Digest::MD5.hexdigest File.read(@overwrite_pretty_file)
		expect(md5_sum).to eq overwrite_md5_sum
		FileUtils.rm @overwrite_target_file
	end

	it "will honor --spaces" do
		beautified_sum = Digest::MD5.hexdigest `bundle exec #{BEAUTIFY_BIN} --spaces #{@space_before}`
		expect(beautified_sum).to eq(@space_after_sum)
	end

	it "will honor the ident_count prefix" do
		beautified_sum = Digest::MD5.hexdigest `bundle exec #{BEAUTIFY_BIN} --indent_count=3 #{@count_before}`
		count_after_sum = Digest::MD5.hexdigest File.read @count_after
		expect(beautified_sum).to eq(count_after_sum)
	end

	it "will use a .ruby-beautify config file" do
		FileUtils.cp "spec/ruby-beautify.dotfile", "tmp/.ruby-beautify"
		Dir.chdir "tmp"
		beautified_sum = Digest::MD5.hexdigest `bundle exec #{BEAUTIFY_BIN} ../#{@space_before}`
		Dir.chdir ".."
		FileUtils.rm "tmp/.ruby-beautify"
		expect(beautified_sum).to eq(@space_after_sum)
	end
end
