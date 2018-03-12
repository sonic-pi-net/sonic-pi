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

class TestSGetText
  include GetText
  bindtextdomain("s_", :path => "locale")

  def test_1
    s_("AAA|BBB")
  end

  def test_2
    sgettext("AAA|BBB")
  end

  def test_3
    s_("AAA") #not found
  end

  def test_4
    s_("AAA|CCC") #not found
  end

  def test_5
    s_("AAA|BBB|CCC") #not found
  end

  def test_6
    s_("AAA$BBB", "$") #not found
  end

  def test_7
    s_("AAA$B|BB", "$") #not found
  end

  def test_8
    s_("AAA$B|CC", "$")
  end

  def test_9
    s_("AAA|CCC|BBB") #not found
  end

  def setlocale(locale)
    __setlocale(locale)
  end
end
