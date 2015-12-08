# -*- encoding: utf-8 -*-
# stub: benchmark-ips 2.1.0 ruby lib

d = File.read(File.expand_path("../lib/benchmark/ips.rb", __FILE__))
if d =~ /VERSION = "(\d+\.\d+\.\d+)"/
  version = $1
else
  version = "0.0.1"
end

Gem::Specification.new do |s|
  s.name = "benchmark-ips"
  s.version = version

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib"]
  s.authors = ["Evan Phoenix"]
  s.date = "2015-01-12"
  s.description = "A iterations per second enhancement to Benchmark."
  s.email = ["evan@phx.io"]
  s.extra_rdoc_files = ["History.txt", "Manifest.txt", "README.md"]
  s.files = [".autotest", ".gemtest", "History.txt", "Manifest.txt", "README.md", "Rakefile", "lib/benchmark/compare.rb", "lib/benchmark/ips.rb", "lib/benchmark/ips/job.rb", "lib/benchmark/ips/report.rb", "lib/benchmark/timing.rb", "test/test_benchmark_ips.rb"]
  s.homepage = "https://github.com/evanphx/benchmark-ips"
  s.licenses = ["MIT"]
  s.rdoc_options = ["--main", "README.md"]
  s.rubygems_version = "2.2.2"
  s.summary = "A iterations per second enhancement to Benchmark."
  s.test_files = ["test/test_benchmark_ips.rb"]

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<minitest>, ["~> 5.4"])
      s.add_development_dependency(%q<rdoc>, ["~> 4.0"])
      s.add_development_dependency(%q<hoe>, ["~> 3.13"])
    else
      s.add_dependency(%q<minitest>, ["~> 5.4"])
      s.add_dependency(%q<rdoc>, ["~> 4.0"])
      s.add_dependency(%q<hoe>, ["~> 3.13"])
    end
  else
    s.add_dependency(%q<minitest>, ["~> 5.4"])
    s.add_dependency(%q<rdoc>, ["~> 4.0"])
    s.add_dependency(%q<hoe>, ["~> 3.13"])
  end
end
