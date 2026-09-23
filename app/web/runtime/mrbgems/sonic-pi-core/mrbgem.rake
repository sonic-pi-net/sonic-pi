MRuby::Gem::Specification.new("sonic-pi-core") do |spec|
  spec.license = "MIT"
  spec.author = "Sam Aaron"
  spec.summary = "What the Sonic Pi runtime needs from C: exact float printing, where a syntax error is"
  spec.add_dependency "mruby-compiler"
end
