#!/usr/bin/env ruby
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

$VERBOSE = true

$KCODE = "utf8" unless "".respond_to?(:encoding)

base_dir = File.expand_path(File.join(File.dirname(__FILE__), ".."))
lib_dir = File.join(base_dir, "lib")
test_dir = File.join(base_dir, "test")

$LOAD_PATH.unshift(lib_dir)
$LOAD_PATH.unshift(test_dir)

require "test-unit"
require "test/unit/notify"
require "test/unit/rr"

require "gettext-test-utils"

Dir.chdir(test_dir) do
  Dir.glob("**/test_*.rb").each do |test_file_path|
    require test_file_path
  end
  exit Test::Unit::AutoRunner.run
end
