# Copyright (C) the Rugged contributors.  All rights reserved.
#
# This file is part of Rugged, distributed under the MIT license.
# For full terms see the included LICENSE file.

begin
  RUBY_VERSION =~ /(\d+.\d+)/
  require "rugged/#{$1}/rugged"
rescue LoadError

  # Modifications made for Sonic Pi multi-platform compatibility:
  require 'rbconfig'
  ruby_api = RbConfig::CONFIG['ruby_version']
  require_relative "../../../rb-native/#{ruby_api}/rugged"
end
# End modifications

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
require 'rugged/attributes'
require 'rugged/blob'
require 'rugged/submodule_collection'
