begin
  RUBY_VERSION =~ /(\d+.\d+)/
  require "rugged/#{$1}/rugged"
rescue LoadError
  #require "rugged/rugged"

  # For Sonic Pi multi-platform compatibility:
  require_relative "../../../rb-native/#{RUBY_PLATFORM}/#{RUBY_VERSION}/rugged"
end
require 'rugged/index'
require 'rugged/object'
require 'rugged/commit'
require 'rugged/version'
require 'rugged/repository'
require 'rugged/reference'
require 'rugged/walker'
require 'rugged/tree'
require 'rugged/tag'
require 'rugged/branch'
require 'rugged/diff'
require 'rugged/patch'
require 'rugged/remote'
require 'rugged/credentials'
