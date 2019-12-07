# -*- ruby -*-

require 'rubygems'
require 'hoe'

Hoe.plugin :minitest
Hoe.plugin :git

hoe = Hoe.spec 'benchmark-ips' do
  developer('Evan Phoenix', 'evan@phx.io')

  self.readme_file = 'README.md'

  license "MIT"
end

file "#{hoe.spec.name}.gemspec" => ['Rakefile', "lib/benchmark/ips.rb"] do |t|
  puts "Generating #{t.name}"
  File.open(t.name, 'wb') { |f| f.write hoe.spec.to_ruby }
end

desc "Generate or update the standalone gemspec file for the project"
task :gemspec => ["#{hoe.spec.name}.gemspec"]


# vim: syntax=ruby
