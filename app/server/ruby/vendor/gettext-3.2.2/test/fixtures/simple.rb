# -*- coding: utf-8 -*-

require 'gettext'

class Simple
  include GetText
  bindtextdomain("test1", :path => "locale")

  def test
    _("language")
  end

  def test_formatted_string
    _("one is %d.") % 1
  end
end
