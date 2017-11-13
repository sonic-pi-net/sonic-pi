require 'bundler/gem_tasks'

if RUBY_ENGINE == "ruby"
  require 'rake/extensiontask'

  Rake::ExtensionTask.new 'did_you_mean' do |ext|
    ext.name    = "method_receiver"
    ext.lib_dir = "lib/did_you_mean"
  end
end

require 'rake/testtask'

Rake::TestTask.new do |task|
  task.libs << "test"
  task.pattern = 'test/**/*_test.rb'
  task.verbose = true
  # task.warning = true
end

desc "Run tests"
task test: [:clobber, :compile] if RUBY_ENGINE == 'ruby'
task default: :test

namespace :test do
  namespace :accuracy do
    desc "Download Wiktionary's Simple English data and save it as a dictionary"
    task :prepare do
      sh 'ruby evaluation/dictionary_generator.rb'
    end
  end

  desc "Calculate accuracy of the gems' spell checker"
  task :accuracy do
    if !File.exist?("evaluation/dictionary.yml")
      puts 'Generating dictionary for evaluation:'
      Rake::Task["test:accuracy:prepare"].execute
      puts "\n"
    end

    sh 'bundle exec ruby evaluation/calculator.rb'
  end
end

namespace :benchmark do
  desc "Measure memory usage by the did_you_mean gem"
  task :memory do
    sh 'bundle exec ruby benchmark/memory_usage.rb'
  end
end
