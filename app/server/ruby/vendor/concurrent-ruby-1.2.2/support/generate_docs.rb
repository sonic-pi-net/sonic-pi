# Using Ruby 2.6 to run this as the docs of 1.1.5 need it

root = File.dirname(__dir__)
versions_file = "#{root}/docs-source/signpost.md"
# Note: 1.0.5 and 1.1.4 are too old, and use different rake task names
versions = File.read(versions_file).scan(/\[(\d+\.\d+\.\d+) with/).map(&:first)
versions.reverse!

def sh(*args)
  command = "$ #{args.join(' ')}"
  puts command
  unless system(*args, exception: true)
    raise "Failed: #{command}"
  end
end

sh "rm", "-rf", "site"
sh "mkdir", "site"

versions.each do |version|
  puts
  puts version
  sh "git", "checkout", "v#{version}"
  has_docs = Dir.exist?('docs')

  sh "rm", "-f", "Gemfile.lock"
  sh "bundle", "install"
  sh "bundle", "exec", "rake", "yard:#{version}"

  sh "cp", "-R", "docs/#{version}", "site"
  sh "rm", "-rf", "docs/#{version}"
  sh "git", "restore", "docs" if has_docs
  sh "git", "restore", "docs-source"
end

sh "git", "checkout", "master"

sh "rm", "-f", "Gemfile.lock"
sh "bundle", "install"
sh "bundle", "exec", "rake", "yard"

versions.each do |version|
  sh "cp", "-R", "site/#{version}", "docs/#{version}"
end

sh "rm", "-rf", "site"
