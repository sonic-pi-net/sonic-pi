require "bundler"

require "rake/testtask"
Rake::TestTask.new(:test) do |test|
  test.libs << "lib" << "test"
  test.test_files = FileList["test/**/*_test.rb"].sort
  test.verbose = true
end
