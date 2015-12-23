require 'rubygems'
require 'rubygems/package_task'

FileUtils.ln_s('.', 'src') if !File.symlink?('src')

load './narray.gemspec'

pkgtsk = Gem::PackageTask.new(GEMSPEC) do |pkg|
  pkg.need_zip = true
  pkg.need_tar = true
end

task :default => "gem"

#--
GEMFILE = File.join(pkgtsk.package_dir, GEMSPEC.file_name)

task :install => GEMFILE do
  sh "gem install -V --backtrace #{GEMFILE}"
end

task :push => GEMFILE do
  sh "gem push #{GEMFILE}"
end
