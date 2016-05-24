# -*- coding: utf-8 -*-
#
# Copyright (C) 2012  Haruka Yoshihara <yoshihara@clear-code.com>
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
include GetText

class MessageFixture
  bindtextdomain("rubyparser", :path => "locale")

  def test_1
    _("aaa")
  end

  def test_2
    _("aaa\n")
  end

  def test_3
    _("bbb\nccc")
  end

  def test_4
     _("bbb
ccc
ddd
")
  end

  def test_5
    _("eee")
  end

  def test_6
    _("eee") + "foo" + _("fff")
  end

  def test_7
    _("ggg"\
      "hhh"\
      "iii")
  end

  def test_8
    _('a"b"c"')
  end

  def test_9
    _("d\"e\"f\"")
  end

  def test_10
    _("jjj") +
    _("kkk")
  end

  def test_11
    _("lll" + "mmm")
  end

  def test_12
    puts _(msg), "ppp"  #Ignored
  end

  def test_13
    _("nnn\n" +
      "ooo")
  end
  def test_14
    _("\#")
  end

  def test_15
    _('#')
  end

  def test_16
    _('\taaa')
  end

  def test_17
    translated = _(<<EOF
Here document1
Here document2
EOF
)
    translated
  end

  def test_18
    "<div>#{_('in_quote')}</div>"
  end

  def about
    puts(
      # TRANSLATORS: This is a proper name.  See the gettext
      # manual, section Names.  Note this is actually a non-ASCII
      # name: The first name is (with Unicode escapes)
      # "Fran\u00e7ois" or (with HTML entities) "Fran&ccedil;ois".
      # Pronunciation is like "fraa-swa pee-nar".
      # This is an example from GNU gettext documentation.
      _("Francois Pinard"))

    puts(
      _("No TRANSLATORS comment"))

    puts(
      # This comment should not be extracted because it does
      # not start with 'TRANSLATORS:'
      _('self explaining'))
  end

  def test_includeing_sharp
    _("This is a # including string.")
  end
end

module ActionController
  class Base
  end
end
class ApplicationController < ActionController::Base
  "#{Time.now.strftime('%m/%d')}"
end
