# -*- coding: utf-8 -*-

require 'gettext'

class Simple
  include GetText
  bindtextdomain("test1", :path => Helper::Path.locale_path)

  def test
    _("language")
  end

  def test_formatted_string
    _("one is %d.") % 1
  end

  def test_plural
    n_("There is an apple.", "There are %{num} apples.", 5) % {:num => 5}
  end
end
