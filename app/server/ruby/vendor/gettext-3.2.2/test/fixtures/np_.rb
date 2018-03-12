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

class TestNPGetText
  include GetText
  bindtextdomain("np_", :path => "locale")

  def test_1
    [np_("Magazine", "a book", "%{num} books", 1),
     np_("Magazine", "a book", "%{num} books", 2)]
  end

  def test_2
    [npgettext("Magazine", "a book", "%{num} books", 1),
     npgettext("Magazine", "a book", "%{num} books", 2)]
  end

  def test_3
    [np_("Hardcover", "a book", "%{num} books", 1),
     np_("Hardcover", "a book", "%{num} books", 2)]
  end

  def test_4
    [np_("Magaine", "I have a magazine", "I have %{num} magazines", 1),
     np_("Magaine", "I have a magazine", "I have %{num} magazines", 2)]
  end

  def test_5
    [np_("Hardcover", "a picture", "%{num} pictures", 1),
     np_("Hardcover", "a picture", "%{num} pictures", 2)]  #not found.
  end
end
