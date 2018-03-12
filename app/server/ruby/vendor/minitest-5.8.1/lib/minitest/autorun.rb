begin
  require "rubygems"
  gem "minitest"
rescue Gem::LoadError
  # do nothing
end

require "minitest"
require "minitest/spec"
require "minitest/mock"

Minitest.autorun
