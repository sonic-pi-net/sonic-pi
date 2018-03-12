# -*- coding: utf-8 -*-
#
# Copyright (C) 2010  masone (Christian Felder) <ema@rh-productions.ch>
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

require 'gettext'

class TestNSGetText
  include GetText
  bindtextdomain("ns_", :path => "locale")

  def test_1
    [ns_("AAA|BBB", "CCC", 1), ns_("AAA|BBB", "CCC", 2)]
  end

  def test_2
    [nsgettext("AAA|BBB", "CCC", 1), nsgettext("AAA|BBB", "CCC", 2)]
  end

  def test_3
    [ns_("AAA", "BBB", 1), ns_("AAA", "BBB", 2)] #not found
  end

  def test_4
    [ns_("AAA|CCC", "DDD", 1), ns_("AAA|CCC", "DDD", 2)] #not found
  end

  def test_5
    [ns_("AAA|BBB|CCC", "DDD", 1), ns_("AAA|BBB|CCC", "DDD", 2)] #not found
  end

  def test_6
    [ns_("AAA$BBB", "CCC", 1, "$"), ns_("AAA$BBB", "CCC", 2, "$")] #not found
  end

  def test_7
    [ns_("AAA$B|BB", "CCC", 1, "$"), ns_("AAA$B|BB", "CCC", 2, "$")] #not found
  end

  def test_8
    [ns_("AAA$B|CC", "DDD", 1, "$"), ns_("AAA$B|CC", "DDD", 2, "$")]
  end

  def test_9
    [ns_("AAA|CCC|BBB", "DDD", 1), ns_("AAA|CCC|BBB", "DDD", 2)] #not found
  end
end
