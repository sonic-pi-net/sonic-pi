# -*- mode: ruby; coding: utf-8 -*-
#
# Copyright (C) 2012-2013  Kouhei Sutou <kou@clear-code.com>
# Copyright (C) 2009  Masao Mutoh
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

base_dir = File.dirname(__FILE__)
$LOAD_PATH.unshift(File.join(base_dir, "lib"))
require "locale/version"

Gem::Specification.new do |s|
  s.name = "locale"
  s.version = Locale::VERSION
  s.summary = 'Ruby-Locale is the pure ruby library which provides basic APIs for localization.'
  s.description = <<-EOD
Ruby-Locale is the pure ruby library which provides basic APIs for localization.
  EOD
  s.authors = ["Kouhei Sutou", "Masao Mutoh"]
  s.email = ["kou@clear-code.com", "mutomasa at gmail.com"]
  s.homepage = "https://github.com/ruby-gettext/locale"
  s.licenses = ["Ruby", "LGPLv3+"]
  s.require_paths = ["lib"]
  Dir.chdir(base_dir) do
    s.files = Dir.glob("{lib,samples}/**/*").find_all do |path|
      File.file?(path)
    end
    s.files += ["COPYING", "ChangeLog", "README.rdoc", "Rakefile"]
    s.files += ["Gemfile", "#{s.name}.gemspec", ".yardopts"]
    s.files += Dir.glob("doc/text/*.*")
    s.test_files = Dir.glob("test/test_*.rb")
  end

  s.add_development_dependency("rake")
  s.add_development_dependency("bundler")
  s.add_development_dependency("yard")
  s.add_development_dependency("redcarpet")
  s.add_development_dependency("test-unit")
  s.add_development_dependency("test-unit-notify")
  s.add_development_dependency("test-unit-rr")
end
