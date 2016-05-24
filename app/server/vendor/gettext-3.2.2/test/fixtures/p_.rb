# -*- coding: utf-8 -*-
#
# Copyright (C) 2010  masone (Christian Felder) <ema@rh-productions.ch>
# Copyright (C) 2009  Vladimir Dobriakov <vladimir@geekq.net>
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

class TestPGetText
  include GetText
  bindtextdomain("p_", :path => "locale")

  def test_1
    p_("AAA", "BBB")
  end

  def test_2
    pgettext("AAA", "BBB")
  end

  def test_3
    pgettext("AAA|BBB", "CCC")
  end

  def test_4
    p_("AAA", "CCC") #not found
  end

  def test_5
    p_("CCC", "BBB")
  end

  def test_6  # not pgettext.
    _("BBB")
  end

  def with_context
    # TRANSLATORS:please translate 'name' in the context of 'program'.
    # Hint: the translation should NOT contain the translation of 'program'.
    p_('program', 'name')
  end
end
