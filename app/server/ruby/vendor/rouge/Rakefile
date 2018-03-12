require 'rake/clean'
require 'pathname'

task :spec do
  spec_files = FileList.new(ENV['files'] || './spec/**/*_spec.rb')
  switch_spec_files = spec_files.map { |x| "-r#{x}" }.join(' ')
  sh "ruby -I./lib -r ./spec/spec_helper #{switch_spec_files} -e Minitest::Unit.autorun"
end

task :doc do
  sh 'bundle exec yard'
end

namespace :doc do
  task :server do
    sh 'bundle exec yard server --reload'
  end

  task :clean do
    sh 'rm -rf ./doc/ ./.yardoc/'
  end
end

CLEAN.include('*.gem')
task :build => [:clean, :spec] do
  puts
  sh "gem build rouge.gemspec"
end

task :default => :spec

Dir.glob(Pathname.new(__FILE__).dirname.join('tasks/*.rake')).each do |f|
  load f
end
