#!/usr/bin/env ruby

require 'net/http'
require 'json'

# http://www.schneems.com/blogs/2015-09-30-reverse-rubygems/

gem_name = "concurrent-ruby"

def rubygems_get(gem_name: "", endpoint: "")
  path = File.join("/api/v1/gems/", gem_name, endpoint).chomp("/") + ".json"
  JSON.parse(Net::HTTP.get("rubygems.org", path))
end

results = rubygems_get(gem_name: gem_name, endpoint: "reverse_dependencies")

weighted_results = {}
results.each do |name|
  begin
    weighted_results[name] = rubygems_get(gem_name: name)["downloads"]
  rescue => e
    puts "#{name} #{e.message}"
  end
end

weighted_results.sort {|(k1, v1), (k2, v2)| v2 <=> v1 }.first(50).each_with_index do |(k, v), i|
  puts "#{i}) #{k}: #{v}"
end
