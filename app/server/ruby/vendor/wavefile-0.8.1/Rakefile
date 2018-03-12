require 'rake/testtask'
require 'rdoc/task'
#$:.push File.expand_path("../tools", __FILE__)

Rake::TestTask.new do |t|
  t.libs << "test"
  t.test_files = FileList['test/**/*_test.rb']
end

RDoc::Task.new do |rdoc|
  rdoc.rdoc_files.include("README.rdoc", "lib")
  rdoc.main = "README.rdoc"
  rdoc.title = "WaveFile Gem - Read/Write *.wav Files with Ruby"
  rdoc.markup = "tomdoc"
  rdoc.rdoc_dir = "doc"
end

namespace :test do
  task :create_fixtures do
    ["valid", "invalid", "unsupported"].each do |subfolder|
      fixtures = Dir.glob("tools/#{subfolder}/*.yml")

      fixtures.each do |fixture|
        basename = File.basename(fixture, ".yml")
        `ruby tools/fixture_writer.rb #{fixture} test/fixtures/#{subfolder}/#{basename}.wav`
      end
    end
  end
end
