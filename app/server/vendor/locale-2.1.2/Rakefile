# -*- mode: ruby; coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@clear-code.com>
#
# License: Ruby's or LGPL
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require 'rake'
require 'rake/testtask'
require "bundler/gem_helper"

base_dir = File.expand_path(File.dirname(__FILE__))

desc "Run tests"
task :default => :test

class Bundler::GemHelper
  undef_method :version_tag
  def version_tag
    version
  end
end

helper = Bundler::GemHelper.new(base_dir)
helper.install
spec = helper.gemspec

desc "Run tests"
task :test do
  options = ARGV - Rake.application.top_level_tasks
  ruby("test/run-test.rb", *options)
end
