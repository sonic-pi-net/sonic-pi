require 'bundler/gem_tasks'

def mri?
  defined?(RUBY_ENGINE) && RUBY_ENGINE == "ruby"
end

desc "run tests"
task default: [:test]

if mri?
  require 'rake/extensiontask'

  Rake::ExtensionTask.new('did_you_mean') do |ext|
    ext.name    = "method_missing"
    ext.lib_dir = "lib/did_you_mean"
  end

  desc "Run tests"
  task :test do
    Rake::Task['compile'].reenable
    Rake::Task['compile'].invoke

    $stdout.puts("\033[33m")
    sh "bundle exec ruby test/all_test.rb"
    $stdout.puts("\033[0m")

    Rake::Task['clobber'].execute
  end

  namespace :test do
    desc "Run tests without re-compiling extensions"
    task :without_compile do
      $stdout.puts("\033[33m")
      sh "bundle exec ruby test/all_test.rb"
      $stdout.puts("\033[0m")
    end
  end
else # for jRuby and Rubinius
  desc "Run tests"
  task :test do
    $stdout.puts("\033[33m")
    sh "bundle exec ruby test/all_test.rb"
    $stdout.puts("\033[0m")
  end
end
