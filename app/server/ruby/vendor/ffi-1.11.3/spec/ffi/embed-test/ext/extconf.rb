#!/usr/bin/env ruby
#
# This file is part of ruby-ffi.
# For licensing, see LICENSE.SPECS
#

if !defined?(RUBY_ENGINE) || RUBY_ENGINE == 'ruby' || RUBY_ENGINE == 'rbx'
  require "mkmf"

  create_makefile("embed_test")
end
